open! Core
open Async
open Neobank_api_types
open Model
module P = Payments_core

let ( let* ) = Deferred.Result.( >>= )
let ok = Deferred.Result.return
let fail p = return (Error p)
let validation r = return (Problem.of_validation r)

let push (ctx : Ctx.t) (t : Transfer.t) =
  Hub.publish ctx.hub t.customer (Transfer (Views.transfer t))
;;

(* Beneficiaries *)

let beneficiaries (ctx : Ctx.t) id =
  let* l = Ctx.read (Any_store.beneficiaries ctx.store id) in
  ok (List.map l ~f:Views.beneficiary)
;;

let add_beneficiary (ctx : Ctx.t) id (r : Api_types.Beneficiary.Request.t) =
  let* rail = validation (Views.rail_of_string r.rail) in
  let* () =
    if String.is_empty (String.strip r.name) || String.length r.name > 70
    then fail (Problem.bad_request "invalid_request" "name must have 1 to 70 characters")
    else ok ()
  in
  let* b =
    validation
      (P.Beneficiary.create
         ~id:(P.Beneficiary.Id.of_string (Ids.next ctx.ids "ben"))
         ~owner:(Customer_id.to_string id)
         ~name:(String.strip r.name)
         ~rail
         ?bic:r.bic
         ?memo:r.memo
         ~now:(Ctx.now ctx)
         r.destination)
  in
  let* () =
    Ctx.commit
      ctx
      [ Put_beneficiary b
      ; Ctx.audit
          ctx
          ~actor:("customer:" ^ Customer_id.to_string id)
          ~action:"beneficiary.add"
          ~object_id:(P.Beneficiary.Id.to_string b.id)
          ~outcome:(Redact.destination r.destination)
          ()
      ]
  in
  ok (Views.beneficiary b)
;;

let owned_beneficiary (ctx : Ctx.t) id beneficiary_id =
  let* b =
    Ctx.find
      ~what:"beneficiary"
      (Any_store.beneficiary ctx.store (P.Beneficiary.Id.of_string beneficiary_id))
  in
  if String.equal b.owner (Customer_id.to_string id)
  then ok b
  else fail (Problem.not_found "beneficiary")
;;

let trust_beneficiary (ctx : Ctx.t) (s : Session.t) beneficiary_id =
  let* () = Auth.require_fresh_sca ctx s in
  let* b = owned_beneficiary ctx s.customer beneficiary_id in
  let b = P.Beneficiary.trust b in
  let* () =
    Ctx.commit
      ctx
      [ Put_beneficiary b
      ; Ctx.audit
          ctx
          ~actor:("customer:" ^ Customer_id.to_string s.customer)
          ~action:"beneficiary.trust"
          ~object_id:beneficiary_id
          ~outcome:"ok"
          ()
      ]
  in
  ok (Views.beneficiary b)
;;

(* Fees *)

let parse_amount (a : Api_types.Amount.t) =
  let open Or_error.Let_syntax in
  let%bind currency = Money.Currency.of_string_or_error a.currency in
  let%bind amount = Money.Amount.of_decimal_string currency a.amount in
  if Money.Amount.is_positive amount
  then Ok amount
  else Or_error.error_string "amount must be positive"
;;

let quote_fee (ctx : Ctx.t) id (r : Api_types.Fee_quote.Request.t) =
  let* account = Accounts.owned ctx id r.account_id in
  let* rail = validation (Views.rail_of_string r.rail) in
  let* amount = validation (parse_amount r.amount) in
  let* q =
    Deferred.map
      (Bank.quote_fee ctx.bank ~from:(Account.id account) ~rail amount)
      ~f:Problem.of_provider
  in
  ok (Views.fee_quote q)
;;

(* Transfers *)

let customer_name ctx id =
  let* case = Onboarding.case ctx id in
  match Kyc_core.Kyc_case.person case with
  | None -> fail (Problem.conflict "kyc_incomplete" "complete the identity check first")
  | Some p ->
    (* A legitimate use of the name: the payer's name travels with the payment (SEPA
       debtor, travel rule originator). *)
    ok (Kyc_core.Pii.reveal p.given_name ^ " " ^ Kyc_core.Pii.reveal p.family_name)
;;

let account_address (a : Account.t) =
  List.find_map a.provider.addresses ~f:(function
    | Crypto { address; _ } -> Some (P.Crypto_address.address address)
    | Iban { iban; _ } -> Some (P.Iban.to_string iban)
    | Lightning_node { node_id } -> Some node_id
    | Uk_account a -> Some (P.Uk_account.to_string a)
    | Us_account a -> Some (P.Us_account.to_string a)
    | Local { fields; _ } ->
      (match List.Assoc.find fields "account_number" ~equal:String.equal with
       | Some number -> Some number
       | None -> Option.map (List.hd fields) ~f:snd))
  |> Option.value ~default:(Account_ref.to_string (Account.id a))
;;

let details
  ctx
  ~id
  ~rail
  ~(account : Account.t)
  ~(beneficiary : P.Beneficiary.t)
  ~payer
  ~wallet_provider
  ~reference
  : P.Transfer.Details.t option Or_error.t
  =
  let open Or_error.Let_syntax in
  match (rail : P.Rail.t), beneficiary.destination with
  | (Sepa_credit | Sepa_instant), Bank_account { iban = creditor_iban; bic } ->
    let%bind debtor_iban =
      Banking_provider.Account.iban account.provider
      |> Or_error.of_option
           ~error:(Error.of_string "the account has no IBAN yet; try again shortly")
    in
    let party name iban bic = { P.Iso20022.Party.name; iban; bic; address = None } in
    let%bind payment_id = P.Iso20022.Payment_id.create ~end_to_end_id:id () in
    let%map remittance =
      if String.is_empty reference
      then Ok P.Iso20022.Remittance.None_given
      else P.Iso20022.Remittance.unstructured reference
    in
    ignore (ctx : Ctx.t);
    Some
      (P.Transfer.Details.Sepa
         (P.Sepa.Details.create
            ~payment_id
            ~debtor:(party payer debtor_iban None)
            ~creditor:(party beneficiary.name creditor_iban bic)
            ~remittance
            ()))
  | Onchain _, Crypto { address; _ } ->
    let person name account identification =
      { P.Travel_rule.Person.name; account; identification; lei = None }
    in
    return
      (Some
         (P.Transfer.Details.Onchain
            { travel_rule =
                Some
                  { originator =
                      person
                        payer
                        (account_address account)
                        (Some (Customer_number (Customer_id.to_string account.customer)))
                  ; beneficiary =
                      person beneficiary.name (P.Crypto_address.address address) None
                  ; counterparty_wallet =
                      (match wallet_provider with
                       | Some provider_name ->
                         Hosted { provider_name; provider_lei = None }
                       | None -> Self_hosted { ownership_verified = false })
                  }
            ; tx_hash = None
            ; block_hash = None
            ; confirmations = 0
            ; finalized = false
            }))
  | _ -> return None
;;

let pick_rail ~(account : Account.t) ~(beneficiary : P.Beneficiary.t) requested =
  let open Or_error.Let_syntax in
  let rails = P.Beneficiary.Destination.rails beneficiary.destination in
  match requested with
  | Some s ->
    let%bind rail = Views.rail_of_string s in
    if List.mem rails rail ~equal:P.Rail.equal
    then Ok rail
    else Or_error.error_s [%message "the beneficiary cannot be paid on this rail" s]
  | None ->
    let fiat = Money.Currency.equal (Account.currency account) Money.Currency.eur in
    List.find rails ~f:(fun (r : P.Rail.t) ->
      match r with
      | Sepa_credit | Sepa_instant -> fiat
      | Onchain _ | Lightning -> not fiat
      | Intraledger -> true
      | Card_scheme -> false
      (* Faster Payments is the default UK rail, standard ACH the default US one. *)
      | Faster_payments ->
        Money.Currency.equal (Account.currency account) Money.Currency.gbp
      | Ach { same_day = false; _ } ->
        Money.Currency.equal (Account.currency account) Money.Currency.usd
      | Chaps | Bacs | Ach { same_day = true; _ } | Rtp | Fedwire -> false)
    |> Or_error.of_option
         ~error:(Error.of_string "no rail reaches this beneficiary from this account")
;;

let risk_context (ctx : Ctx.t) ~(customer : Customer.t) ~case ~available draft ~wallet =
  let* transfer = validation (Risk_rules.Context.Transfer.create ?wallet draft) in
  let* history = Ctx.read (Any_store.transfers ctx.store customer.id ()) in
  let outgoing, incoming =
    List.partition_tf history ~f:(fun (t : Transfer.t) ->
      match t.transfer.direction with
      | Out -> true
      | In -> false)
  in
  let sender_key detail =
    Cryptokit.transform_string
      (Cryptokit.Hexa.encode ())
      (Cryptokit.hash_string (Cryptokit.Hash.sha256 ()) detail)
  in
  ok
    { Risk_rules.Context.customer = Customer_id.to_string customer.id
    ; tier = Risk_policy.tier_of_kyc (Kyc_core.Kyc_case.tier case)
    ; now = Ctx.now ctx
    ; balances = [ available ]
    ; subject = Transfer transfer
    ; history =
        List.filter_map outgoing ~f:(fun t ->
          Risk_rules.Context.Past_transfer.of_transfer t.transfer)
    ; incoming =
        List.filter_map incoming ~f:(fun t ->
          Risk_rules.Context.Past_transfer.of_incoming ~sender_key t.transfer)
    ; card_history = []
    ; geo = Risk_rules.Context.Geo.unknown
    ; session =
        { new_device = false
        ; credentials_changed_at = None
        ; account_opened_at = Some customer.created_at
        ; failed_sca_24h = Auth.failed_sca_24h ctx customer
        }
    }
;;

let binding (t : P.Transfer.t) =
  let payee =
    match t.counterparty with
    | Beneficiary b -> P.Beneficiary.Id.to_string b.id
    | External { detail; _ } -> detail
  in
  P.Sca.Binding.create ~operation:"transfer" ~amount:(P.Transfer.total_debit t) ~payee
;;

let new_challenge (ctx : Ctx.t) (x : P.Transfer.t) ~customer =
  P.Sca.Challenge.create
    ~id:(P.Sca.Id.of_string (Ids.next ctx.ids "sca"))
    ~subject:(Customer_id.to_string customer)
    ~binding:(binding x)
    ~factors:[ Password; Totp ]
    ~now:(Ctx.now ctx)
    ~ttl:ctx.config.challenge_ttl
    ~max_attempts:P.Sca.max_attempts_ceiling
    ()
;;

let initiate_message (ctx : Ctx.t) (id : P.Transfer.Id.t) =
  Store.Write.Enqueue
    (Outbox.create ~id:(Ids.next ctx.ids "msg") ~now:(Ctx.now ctx) (Initiate_transfer id))
;;

let customer_audit (ctx : Ctx.t) customer ~action ~object_id ~outcome =
  Ctx.audit
    ctx
    ~actor:("customer:" ^ Customer_id.to_string customer)
    ~action
    ~object_id
    ~outcome
    ()
;;

(* Moves a transfer that may proceed to SCA, or straight to the provider when risk-rules
   named an exemption. *)
let authorize (ctx : Ctx.t) (record : Transfer.t) ~decision =
  let now = Ctx.now ctx in
  let id = P.Transfer.Id.to_string (Transfer.id record) in
  match Risk_rules.Sca_outcome.of_decision decision with
  | Exempt exemption ->
    let* x =
      return (Problem.of_internal (P.Transfer.exempt record.transfer ~exemption))
    in
    let record = { record with transfer = x; updated_at = now } in
    ok
      ( record
      , [ Store.Write.Put_transfer record
        ; initiate_message ctx x.id
        ; customer_audit
            ctx
            record.customer
            ~action:"transfer.exempt"
            ~object_id:id
            ~outcome:(P.Sca.Exemption.article exemption)
        ] )
  | Required _ | Not_assessed ->
    let* challenge =
      return
        (Problem.of_internal
           (new_challenge ctx record.transfer ~customer:record.customer))
    in
    let* x =
      return
        (Problem.of_internal
           (P.Transfer.require_sca record.transfer ~challenge:challenge.id ~at:now))
    in
    let record =
      { record with transfer = x; challenge = Some challenge; updated_at = now }
    in
    ok
      ( record
      , [ Store.Write.Put_transfer record
        ; customer_audit
            ctx
            record.customer
            ~action:"transfer.challenge"
            ~object_id:id
            ~outcome:"sent"
        ] )
;;

let create_transfer
  (ctx : Ctx.t)
  (s : Session.t)
  ~wallet_provider
  (r : Api_types.Transfer.Request.t)
  =
  let now = Ctx.now ctx in
  let* customer = Ctx.find ~what:"customer" (Any_store.customer ctx.store s.customer) in
  let* account = Accounts.owned ctx s.customer r.account_id in
  let* beneficiary = owned_beneficiary ctx s.customer r.beneficiary_id in
  let* amount = validation (parse_amount r.amount) in
  let* () =
    if Money.Currency.equal (Money.Amount.currency amount) (Account.currency account)
    then ok ()
    else
      fail
        (Problem.bad_request
           "currency_mismatch"
           "the amount must be in the account's currency")
  in
  let* rail = validation (pick_rail ~account ~beneficiary r.rail) in
  let* case = Onboarding.case ctx s.customer in
  let* payer = customer_name ctx s.customer in
  let id = Ids.next ctx.ids "tr" in
  let reference = Option.value r.reference ~default:"" in
  let* details =
    validation
      (details ctx ~id ~rail ~account ~beneficiary ~payer ~wallet_provider ~reference)
  in
  let* quote =
    Deferred.map
      (Bank.quote_fee ctx.bank ~from:(Account.id account) ~rail amount)
      ~f:Problem.of_provider
  in
  let* balance =
    Deferred.map (Bank.balance ctx.bank (Account.id account)) ~f:Problem.of_provider
  in
  let* () =
    match Money.Amount.compare_value balance.available quote.total with
    | Ok n when n >= 0 -> ok ()
    | _ ->
      fail
        (Problem.unprocessable
           "insufficient_funds"
           "the available balance does not cover the amount and fee")
  in
  let* () =
    (* The travel rule refuses more than EUR 1000 to a self-hosted wallet whose ownership
       was not proven; say so plainly instead of as a validation error. *)
    match beneficiary.destination, wallet_provider, Ctx.eur_value ctx amount with
    | Crypto _, None, Some eur
      when Money.Amount.compare_value eur P.Travel_rule.self_hosted_threshold_eur
           |> Result.ok
           |> Option.exists ~f:(fun n -> n > 0) ->
      fail
        (Problem.unprocessable
           "wallet_ownership_required"
           "prove you control this wallet before sending more than EUR 1000 to it, or \
            name the provider that hosts it")
    | _ -> ok ()
  in
  let* draft =
    validation
      (P.Transfer.draft
         ~id:(P.Transfer.Id.of_string id)
         ~rail
         ~account:
           (Ledger.Account_id.of_string
              ("app:" ^ Account_ref.to_string (Account.id account)))
         ~beneficiary
         ~amount
         ~fee:quote.fee
         ~reference
         ?details
         ?eur_value:(Ctx.eur_value ctx amount)
         ~now
         ())
  in
  let wallet =
    match beneficiary.destination with
    | Crypto _ ->
      Some
        { Risk_rules.Context.Wallet.self_hosted = Option.is_none wallet_provider
        ; ownership_verified = false
        }
    | Bank_account _ | Lightning _ | Internal _ | Uk_account _ | Us_account _ | Local _ ->
      None
  in
  let* context =
    risk_context ctx ~customer ~case ~available:balance.available draft ~wallet
  in
  let decision = Risk_rules.Policy.evaluate ctx.policy context in
  Metrics.incr
    ctx.metrics
    "risk_decisions"
    ~labels:[ "severity", Risk_rules.Severity.to_string decision.severity ];
  let record : Transfer.t =
    { transfer = draft
    ; customer = s.customer
    ; account = Account.id account
    ; beneficiary = Some beneficiary.id
    ; risk = Some { context; decision }
    ; hold = Not_held
    ; challenge = None
    ; provider_challenge = None
    ; created_at = now
    ; updated_at = now
    }
  in
  match decision.severity with
  | Block ->
    let* x =
      return (Problem.of_internal (P.Transfer.transition draft ~to_:Cancelled ~at:now))
    in
    let record = { record with transfer = x } in
    let* () =
      Ctx.commit
        ctx
        [ Put_transfer record
        ; customer_audit
            ctx
            s.customer
            ~action:"transfer.refused"
            ~object_id:id
            ~outcome:"blocked"
        ]
    in
    fail
      (Problem.unprocessable
         "payment_refused"
         (Option.value
            (Risk_rules.Decision.customer_message decision)
            ~default:"this payment cannot be made"))
  | Review ->
    let review_id = Ids.next ctx.ids "rev" in
    let record = { record with hold = Under_review { review = review_id } } in
    let review : Review.t =
      { id = review_id
      ; reference = id
      ; customer = s.customer
      ; risk = { context; decision }
      ; submitted_at = now
      ; status = Pending
      ; notes = []
      }
    in
    let* () =
      Ctx.commit
        ctx
        [ Put_transfer record
        ; Put_review review
        ; customer_audit
            ctx
            s.customer
            ~action:"transfer.held"
            ~object_id:id
            ~outcome:review_id
        ]
    in
    Metrics.incr ctx.metrics "reviews_opened";
    ok (`Accepted, Views.transfer record)
  | Allow | Challenge ->
    let* (_ : Transfer.t), writes = authorize ctx record ~decision in
    let* () = Ctx.commit ctx writes in
    let%bind () = Ctx.settle ctx in
    let* record = Ctx.find ~what:"transfer" (Any_store.transfer ctx.store draft.id) in
    ok (`Created, Views.transfer record)
;;

let owned_transfer (ctx : Ctx.t) customer id =
  let* t =
    Ctx.find ~what:"transfer" (Any_store.transfer ctx.store (P.Transfer.Id.of_string id))
  in
  if Customer_id.equal t.customer customer
  then ok t
  else fail (Problem.not_found "transfer")
;;

let with_transfer_lock (ctx : Ctx.t) id f =
  Keyed_lock.with_ ctx.locks ~key:("transfer:" ^ id) f
;;

let restart_sca (ctx : Ctx.t) customer id =
  with_transfer_lock ctx id (fun () ->
    let* record = owned_transfer ctx customer id in
    let now = Ctx.now ctx in
    let challenge_open =
      Option.exists record.challenge ~f:(fun c -> P.Sca.Challenge.is_open c ~now)
    in
    let verified =
      Option.exists record.challenge ~f:(fun c ->
        match c.status with
        | Verified _ -> true
        | Pending | Failed _ -> false)
    in
    match record.hold, record.transfer.status with
    | Under_review _, _ -> fail (Problem.conflict "under_review" "under review")
    | Refused _, _ -> fail (Problem.conflict "refused" "this payment could not be made")
    | _, (Draft | Awaiting_sca) when not (challenge_open || verified) ->
      let* challenge =
        return (Problem.of_internal (new_challenge ctx record.transfer ~customer))
      in
      let* x =
        match record.transfer.status with
        | Draft ->
          return
            (Problem.of_internal
               (P.Transfer.require_sca record.transfer ~challenge:challenge.id ~at:now))
        | _ ->
          (* Still Awaiting_sca: point the authorization at the new challenge (the
             transition table has no Awaiting_sca -> Awaiting_sca step). *)
          ok { record.transfer with authorization = Some (Sca challenge.id) }
      in
      let record =
        { record with transfer = x; challenge = Some challenge; updated_at = now }
      in
      let* () = Ctx.commit ctx [ Put_transfer record ] in
      ok (Views.transfer record)
    | _ ->
      fail (Problem.conflict "nothing_to_confirm" "this transfer needs no new challenge"))
;;

let sca_failed ?sca message =
  Problem.create ?sca `Unprocessable_entity "sca_failed" message
;;

let confirm_ours
  (ctx : Ctx.t)
  (record : Transfer.t)
  (challenge : P.Sca.Challenge.t)
  ~password
  ~code
  =
  let* customer =
    Ctx.find ~what:"customer" (Any_store.customer ctx.store record.customer)
  in
  let* () = Auth.check_not_locked ctx customer in
  let%bind factors = Auth.check_factors ctx customer ~password ~totp:code in
  let correct, customer' =
    match factors with
    | `Ok c -> true, Auth.record_success c
    | `Wrong -> false, customer
  in
  let module Verifier = struct
    let check (_ : P.Sca.Challenge.t) ~kind:(_ : P.Sca.Kind.t) ~response:(_ : string) =
      return correct
    ;;
  end
  in
  let now = Ctx.now ctx in
  let verify challenge kind =
    P.Sca.verify
      (module Verifier)
      challenge
      ~binding:(binding record.transfer)
      ~kind
      ~response:""
      ~now
  in
  (* Both factors are checked before either is recorded, and a wrong answer counts once
     against the password, so the outcome never says which factor was wrong. *)
  let%bind challenge, outcome =
    let%bind challenge, outcome = verify challenge Password in
    match outcome with
    | Factor_accepted _ -> verify challenge Totp
    | _ -> return (challenge, outcome)
  in
  let id = P.Transfer.Id.to_string (Transfer.id record) in
  let record = { record with challenge = Some challenge; updated_at = now } in
  let audit outcome =
    customer_audit ctx record.customer ~action:"transfer.sca" ~object_id:id ~outcome
  in
  match outcome with
  | Verified | Already_verified ->
    let* () =
      Ctx.commit
        ctx
        [ Put_transfer record
        ; Update_customer customer'
        ; initiate_message ctx record.transfer.id
        ; audit "verified"
        ]
    in
    Metrics.incr ctx.metrics "transfers_authorized";
    ok record
  | Wrong_response { attempts_left } ->
    let customer', locked = Auth.record_failure ctx customer in
    let* () =
      Ctx.commit ctx [ Put_transfer record; Update_customer customer'; audit "wrong" ]
    in
    (match locked with
     | `Locked ->
       fail (Problem.create `Locked "locked" "too many failed attempts; try again later")
     | `Not_locked ->
       fail
         (sca_failed
            ~sca:(Views.sca_challenge challenge)
            (sprintf "the code or password is wrong; %d attempts left" attempts_left)))
  | Rejected Expired ->
    let* () = Ctx.commit ctx [ Put_transfer record; audit "expired" ] in
    fail
      (Problem.unprocessable
         "sca_expired"
         "the challenge expired; ask for a new one (POST /transfers/:id/sca)")
  | Rejected (Too_many_attempts | Cancelled) ->
    let* x =
      return
        (Problem.of_internal
           (P.Transfer.transition record.transfer ~to_:(Failed Sca_failed) ~at:now))
    in
    let record = { record with transfer = x } in
    let* () = Ctx.commit ctx [ Put_transfer record; audit "failed" ] in
    fail (sca_failed "too many wrong answers; the payment was stopped")
  | Binding_mismatch ->
    fail (Problem.conflict "binding_mismatch" "the challenge is for another payment")
  | Factor_accepted _ | Not_a_factor _ ->
    fail (Problem.internal (Error.of_string "unexpected SCA outcome"))
;;

let confirm_provider (ctx : Ctx.t) (record : Transfer.t) (c : Provider_challenge.t) ~code =
  let* result =
    Deferred.map
      (Bank.confirm_sca ctx.bank c.id ~code:(Banking_provider.Sensitive.of_string code))
      ~f:Problem.of_provider
  in
  let now = Ctx.now ctx in
  let id = P.Transfer.Id.to_string (Transfer.id record) in
  let audit outcome =
    customer_audit
      ctx
      record.customer
      ~action:"transfer.provider_sca"
      ~object_id:id
      ~outcome
  in
  match result with
  | Accepted ->
    let record = { record with provider_challenge = None; updated_at = now } in
    let* () = Ctx.commit ctx [ Put_transfer record; audit "accepted" ] in
    ok record
  | Wrong_code { attempts_left } ->
    let c = { c with attempts_left } in
    let record = { record with provider_challenge = Some c; updated_at = now } in
    let* () = Ctx.commit ctx [ Put_transfer record; audit "wrong" ] in
    fail
      (sca_failed
         ~sca:(Views.provider_challenge c)
         (sprintf "the code is wrong; %d attempts left" attempts_left))
  | Failed _ ->
    let* x =
      return
        (Problem.of_internal
           (P.Transfer.transition record.transfer ~to_:(Failed Sca_failed) ~at:now))
    in
    let record =
      { record with transfer = x; provider_challenge = None; updated_at = now }
    in
    let* () = Ctx.commit ctx [ Put_transfer record; audit "failed" ] in
    fail (sca_failed "the bank stopped the payment after failed confirmations")
;;

let confirm_sca (ctx : Ctx.t) customer id (r : Api_types.Transfer.Confirm_request.t) =
  let* record =
    with_transfer_lock ctx id (fun () ->
      let* record = owned_transfer ctx customer id in
      let pending_ours =
        Option.filter record.challenge ~f:(fun c ->
          match c.status with
          | Pending -> true
          | Verified _ | Failed _ -> false)
      in
      match pending_ours, record.provider_challenge with
      | Some challenge, _ ->
        (match r.password with
         | None ->
           fail
             (Problem.bad_request
                "invalid_request"
                "password and code are both required to confirm this payment")
         | Some password -> confirm_ours ctx record challenge ~password ~code:r.code)
      | None, Some c -> confirm_provider ctx record c ~code:r.code
      | None, None ->
        fail (Problem.conflict "nothing_to_confirm" "this transfer waits for no code"))
  in
  let%bind () = Ctx.settle ctx in
  let* record =
    Ctx.find ~what:"transfer" (Any_store.transfer ctx.store (Transfer.id record))
  in
  push ctx record;
  ok (Views.transfer record)
;;

let cancel_transfer (ctx : Ctx.t) customer id =
  with_transfer_lock ctx id (fun () ->
    let* record = owned_transfer ctx customer id in
    match record.transfer.status, record.hold with
    | (Draft | Awaiting_sca), (Not_held | Released _) ->
      let now = Ctx.now ctx in
      let* x =
        return
          (Problem.of_internal
             (P.Transfer.transition record.transfer ~to_:Cancelled ~at:now))
      in
      let record = { record with transfer = x; updated_at = now } in
      let* () =
        Ctx.commit
          ctx
          [ Put_transfer record
          ; customer_audit
              ctx
              customer
              ~action:"transfer.cancel"
              ~object_id:id
              ~outcome:"ok"
          ]
      in
      push ctx record;
      ok (Views.transfer record)
    | _ ->
      fail
        (Problem.conflict
           "not_cancellable"
           "only a payment not yet sent to the bank can be cancelled"))
;;

let get_transfer ctx customer id =
  let* record = owned_transfer ctx customer id in
  ok (Views.transfer record)
;;

let list_transfers (ctx : Ctx.t) customer =
  let* l = Ctx.read (Any_store.transfers ctx.store customer ()) in
  ok (List.map l ~f:Views.transfer)
;;

let initiate (ctx : Ctx.t) id =
  let key = P.Transfer.Id.to_string id in
  with_transfer_lock ctx key (fun () ->
    let open Deferred.Or_error.Let_syntax in
    match%bind Any_store.transfer ctx.store id with
    | None -> Deferred.Or_error.error_s [%message "no such transfer" key]
    | Some record ->
      let authorized =
        match record.transfer.authorization with
        | Some (Exempt _) -> true
        | Some (Sca _) ->
          Option.exists record.challenge ~f:(fun c ->
            match c.status with
            | Verified _ -> true
            | Pending | Failed _ -> false)
        | None -> false
      in
      (match record.transfer.status with
       | (Draft | Awaiting_sca) when Option.is_none record.provider_challenge ->
         if not authorized
         then Deferred.Or_error.error_s [%message "transfer is not authorized" key]
         else (
           (* The provider takes the payment as drafted: our SCA bookkeeping
              (Awaiting_sca) is ours, so it gets the transfer back in Draft, with the
              authorization recorded. *)
           let as_drafted = { record.transfer with status = Draft } in
           let%bind answer =
             Bank.initiate_transfer ctx.bank ~from:record.account as_drafted
           in
           let now = Ctx.now ctx in
           let%bind record =
             match answer with
             | Done info ->
               let%map x =
                 Deferred.return
                   (P.Transfer.submit
                      record.transfer
                      ~provider_ref:info.provider_ref
                      ~at:now
                      ())
               in
               { record with transfer = x; updated_at = now }
             | Challenge c ->
               return
                 { record with
                   provider_challenge = Some (Provider_challenge.of_provider c)
                 ; updated_at = now
                 }
           in
           let%map (_ : Store.Commit_result.t) =
             Any_store.commit
               ctx.store
               [ Put_transfer record
               ; Ctx.audit
                   ctx
                   ~actor:"system"
                   ~on_behalf_of:(Customer_id.to_string record.customer)
                   ~action:"transfer.initiate"
                   ~object_id:key
                   ~outcome:
                     (match answer with
                      | Done _ -> "submitted"
                      | Challenge _ -> "provider_sca")
                   ()
               ]
           in
           push ctx record)
       | _ ->
         (* Already sent (or finished, or waiting for the provider's code): a resent
            outbox message has nothing left to do. *)
         return ()))
;;

let apply_update (ctx : Ctx.t) (record : Transfer.t) update ~at =
  let open Or_error.Let_syntax in
  let eur_value = Ctx.eur_value ctx record.transfer.amount in
  match%map
    Banking_provider.Transfer_update.apply update record.transfer ?eur_value ~at ()
  with
  | Stale -> None
  | Changed x ->
    let provider_challenge =
      match x.status with
      | Draft | Awaiting_sca -> record.provider_challenge
      | _ -> None
    in
    Some { record with transfer = x; provider_challenge; updated_at = Ctx.now ctx }
;;

(* Reviews *)

let reviews (ctx : Ctx.t) ~pending_only =
  let* l = Ctx.read (Any_store.reviews ctx.store ~pending_only) in
  ok (List.map l ~f:Views.review)
;;

let decide_review (ctx : Ctx.t) review_id (r : Api_types.Review.Decide_request.t) =
  let* review = Ctx.find ~what:"review" (Any_store.review ctx.store review_id) in
  let* () =
    if Review.is_pending review
    then ok ()
    else fail (Problem.conflict "already_decided" "this review was already decided")
  in
  let operator = String.strip r.operator in
  let* () =
    if String.is_empty operator
    then fail (Problem.bad_request "invalid_request" "operator is required")
    else ok ()
  in
  let* verdict =
    match String.lowercase r.verdict with
    | "approve" -> ok `Approve
    | "reject" -> ok `Reject
    | _ -> fail (Problem.bad_request "invalid_request" "verdict is approve or reject")
  in
  let now = Ctx.now ctx in
  let notes = review.notes @ Option.to_list r.note in
  let* () =
    with_transfer_lock ctx review.reference (fun () ->
      let* record =
        Ctx.find
          ~what:"transfer"
          (Any_store.transfer ctx.store (P.Transfer.Id.of_string review.reference))
      in
      let audit outcome =
        Ctx.audit
          ctx
          ~actor:("operator:" ^ operator)
          ~on_behalf_of:(Customer_id.to_string record.customer)
          ~action:"review.decide"
          ~object_id:review_id
          ~outcome
          ()
      in
      match verdict, review.status with
      | `Approve, Pending ->
        (* Four eyes: the first approval only records the operator. *)
        let review =
          { review with status = First_approval { operator; at = now }; notes }
        in
        let* () = Ctx.commit ctx [ Put_review review; audit "first_approval" ] in
        ok ()
      | `Approve, First_approval { operator = first; _ } when String.equal first operator
        ->
        fail
          (Problem.conflict
             "same_operator"
             "a second, different operator must approve the release")
      | `Approve, _ ->
        let record =
          { record with hold = Released { review = review_id }; updated_at = now }
        in
        let* record, writes = authorize ctx record ~decision:review.risk.decision in
        let review = { review with status = Approved { operator; at = now }; notes } in
        let* () =
          Ctx.commit ctx ((Store.Write.Put_review review :: writes) @ [ audit "approved" ])
        in
        push ctx record;
        ok ()
      | `Reject, _ ->
        let* x =
          return
            (Problem.of_internal
               (P.Transfer.transition record.transfer ~to_:Cancelled ~at:now))
        in
        let record =
          { record with
            transfer = x
          ; hold = Refused { review = review_id }
          ; updated_at = now
          }
        in
        let review = { review with status = Rejected { operator; at = now }; notes } in
        let* () =
          Ctx.commit ctx [ Put_review review; Put_transfer record; audit "rejected" ]
        in
        push ctx record;
        ok ())
  in
  let%bind () = Ctx.settle ctx in
  let* review = Ctx.find ~what:"review" (Any_store.review ctx.store review_id) in
  ok (Views.review review)
;;
