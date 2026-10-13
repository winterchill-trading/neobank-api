open! Core
open Async
open Neobank_api_types
open Model

let ( let* ) = Deferred.Result.( >>= )
let ok = Deferred.Result.return
let fail p = return (Error p)

let case (ctx : Ctx.t) id =
  let* events = Ctx.read (Any_store.kyc_events ctx.store id) in
  return
    (Problem.of_internal
       (Kyc_core.Kyc_case.of_events ~policy:ctx.config.kyc_policy id events))
;;

let status ctx id =
  let* case = case ctx id in
  ok (Views.kyc (Kyc_core.Kyc_case.customer_view case))
;;

let step_of_string s : Kyc_core.Verification.Step.t Or_error.t =
  match String.lowercase (String.strip s) with
  | "email" -> Ok Email
  | "mobile" -> Ok Mobile
  | "identity_document" -> Ok Identity_document
  | "liveness" -> Ok Liveness
  | "address_proof" -> Ok Address_proof
  | "source_of_funds" -> Ok Source_of_funds
  | _ -> Or_error.error_s [%message "unknown verification step" s]
;;

let step_name step =
  String.lowercase (Sexp.to_string [%sexp (step : Kyc_core.Verification.Step.t)])
;;

let provider_ref (c : Customer.t) =
  match c.provider_ref with
  | Some r -> ok r
  | None ->
    fail
      (Problem.conflict
         "provider_pending"
         "the account is still being set up at the bank; try again in a moment")
;;

let start (ctx : Ctx.t) (c : Customer.t) (r : Api_types.Kyc.Start_request.t) =
  let* step = return (Problem.of_validation (step_of_string r.step)) in
  let* provider = provider_ref c in
  let* session =
    Deferred.map (Bank.start_verification ctx.bank provider step) ~f:Problem.of_provider
  in
  Metrics.incr ctx.metrics "kyc_steps_started" ~labels:[ "step", step_name step ];
  ok
    (match session with
     | Code_sent { step; expires_at } ->
       { Api_types.Kyc.Session.step = step_name step
       ; kind = "code_sent"
       ; sdk_token = None
       ; expires_at = Views.time expires_at
       }
     | Hosted { step; token; expires_at } ->
       { step = step_name step
       ; kind = "hosted"
       ; (* The token exists to be handed to the provider's SDK in the customer's browser;
            this response is the one place it is revealed. *)
         sdk_token = Some (Banking_provider.Sensitive.reveal token)
       ; expires_at = Views.time expires_at
       })
;;

let complete (ctx : Ctx.t) (c : Customer.t) (r : Api_types.Kyc.Complete_request.t) =
  let* step = return (Problem.of_validation (step_of_string r.step)) in
  let* provider = provider_ref c in
  let* state =
    Deferred.map
      (Bank.complete_verification
         ctx.bank
         provider
         step
         ~code:(Banking_provider.Sensitive.of_string r.code))
      ~f:Problem.of_provider
  in
  match state with
  | Failed _ -> fail (Problem.unprocessable "wrong_code" "the code is wrong or expired")
  | Pending | Sent | Verified _ | Expired -> status ctx c.id
;;

let create_provider_customer (ctx : Ctx.t) id =
  Keyed_lock.with_ ctx.locks ~key:(Customer_id.to_string id) (fun () ->
    let open Deferred.Or_error.Let_syntax in
    match%bind Any_store.customer ctx.store id with
    | None -> Deferred.Or_error.error_s [%message "no such customer" (id : Customer_id.t)]
    | Some { provider_ref = Some _; _ } -> return ()
    | Some c ->
      let%bind events = Any_store.kyc_events ctx.store id in
      let%bind case =
        Deferred.return
          (Kyc_core.Kyc_case.of_events ~policy:ctx.config.kyc_policy id events)
      in
      let%bind person =
        Deferred.return
          (Kyc_core.Kyc_case.person case
           |> Or_error.of_option ~error:(Error.of_string "the case has no person"))
      in
      let%bind created = Bank.create_customer ctx.bank ~customer_id:id ~person in
      let%bind result =
        Any_store.commit
          ctx.store
          [ Update_customer { c with provider_ref = Some created.id }
          ; Ctx.audit
              ctx
              ~actor:"system"
              ~on_behalf_of:(Customer_id.to_string id)
              ~action:"provider.customer_created"
              ~object_id:(Customer_ref.to_string created.id)
              ~outcome:"ok"
              ()
          ]
      in
      (match result with
       | Committed | Duplicate_event -> return ()
       | Conflict c -> Deferred.Or_error.error_s [%message "conflict" c]))
;;

let open_accounts (ctx : Ctx.t) id =
  Keyed_lock.with_ ctx.locks ~key:(Customer_id.to_string id) (fun () ->
    let open Deferred.Or_error.Let_syntax in
    let%bind c = Any_store.customer ctx.store id in
    let%bind provider =
      match Option.bind c ~f:(fun c -> c.provider_ref) with
      | Some r -> return r
      | None -> Deferred.Or_error.error_string "no provider customer yet"
    in
    let%bind existing = Bank.accounts ctx.bank provider in
    let missing =
      List.filter ctx.config.currencies ~f:(fun currency ->
        not
          (List.exists existing ~f:(fun (a : Banking_provider.Account.t) ->
             Money.Currency.equal a.currency currency)))
    in
    let%bind opened =
      Deferred.Or_error.List.map ~how:`Sequential missing ~f:(fun currency ->
        Bank.open_account ctx.bank provider currency)
    in
    let%bind enriched =
      Deferred.Or_error.List.map
        ~how:`Sequential
        (existing @ opened)
        ~f:(fun (a : Banking_provider.Account.t) ->
          (* Some providers assign an IBAN or address minutes later; keep the account as
             it is and let the account event bring the address. *)
          match%map.Deferred Bank.enrich_account ctx.bank a.id with
          | Ok a -> Ok a
          | Error _ -> Ok a)
    in
    let now = Ctx.now ctx in
    let%bind result =
      Any_store.commit
        ctx.store
        (List.map enriched ~f:(fun provider ->
           Store.Write.Put_account { customer = id; provider; updated_at = now })
         @ [ Ctx.audit
               ctx
               ~actor:"system"
               ~on_behalf_of:(Customer_id.to_string id)
               ~action:"accounts.opened"
               ~object_id:(Customer_id.to_string id)
               ~outcome:(sprintf "%d accounts" (List.length enriched))
               ()
           ])
    in
    ignore (result : Store.Commit_result.t);
    List.iter enriched ~f:(fun provider ->
      Hub.publish
        ctx.hub
        id
        (Account (Views.account { customer = id; provider; updated_at = now })));
    return ())
;;

let writes_for_event (ctx : Ctx.t) (c : Customer.t) (event : Banking_provider.Event.t) =
  let* case = case ctx c.id in
  match
    Banking_provider.Event.to_kyc_event
      event
      ~customer_id:c.id
      ~provider:(Bank.name ctx.bank)
  with
  | None -> ok ([], None)
  | Some kyc_event ->
    (match Kyc_core.Kyc_case.apply case kyc_event with
     | Error e ->
       Metrics.incr ctx.metrics "kyc_events_refused";
       Log.Global.info_s
         [%message
           "the KYC case refused a provider event"
             ~customer:(c.id : Customer_id.t)
             (e : Error.t)];
       ok ([], None)
     | Ok case' ->
       let was_approved =
         Kyc_core.Status.equal (Kyc_core.Kyc_case.status case) Approved
       in
       let approved = Kyc_core.Status.equal (Kyc_core.Kyc_case.status case') Approved in
       let open_accounts =
         if approved && not was_approved
         then
           [ Store.Write.Enqueue
               (Outbox.create
                  ~id:(Ids.next ctx.ids "msg")
                  ~now:(Ctx.now ctx)
                  (Open_accounts c.id))
           ]
         else []
       in
       ok
         ( Store.Write.Kyc_event kyc_event :: open_accounts
         , Some (Kyc_core.Kyc_case.customer_view case') ))
;;
