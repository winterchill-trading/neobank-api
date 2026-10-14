open! Core
open Async
open Neobank_api_types
open Model

let ( let* ) = Deferred.Result.( >>= )
let ok = Deferred.Result.return
let fail p = return (Error p)
let lock (ctx : Ctx.t) id f = Keyed_lock.with_ ctx.locks ~key:("card:" ^ id) f
let push (ctx : Ctx.t) (c : Card.t) = Hub.publish ctx.hub c.customer (Card (Views.card c))

let issue (ctx : Ctx.t) (s : Session.t) ~request_id (r : Api_types.Card.Issue_request.t) =
  match%bind Ctx.read (Any_store.card_by_request ctx.store ~request_id) with
  | Error p -> fail p
  | Ok (Some card) when Customer_id.equal card.customer s.customer -> ok (Views.card card)
  | Ok (Some _) ->
    fail (Problem.conflict "card_request_exists" "use another idempotency key")
  | Ok None ->
    let* account = Accounts.owned ctx s.customer r.account_id in
    let* customer = Ctx.find ~what:"customer" (Any_store.customer ctx.store s.customer) in
    let* provider =
      match customer.provider_ref with
      | Some p -> ok p
      | None ->
        fail (Problem.conflict "provider_pending" "the account is still being set up")
    in
    let* info =
      Deferred.map
        (Bank.issue_virtual_card
           ctx.bank
           ~request_id
           provider
           ~account:(Account.id account))
        ~f:Problem.of_provider
    in
    let card = Card.of_info info ~customer:s.customer ~request_id ~now:(Ctx.now ctx) in
    (match%bind
       Any_store.commit
         ctx.store
         [ Insert_card card
         ; Ctx.audit
             ctx
             ~actor:("customer:" ^ Customer_id.to_string s.customer)
             ~action:"card.issue"
             ~object_id:(Card_core.Ids.Card_id.to_string card.id)
             ~outcome:"ok"
             ()
         ]
     with
     | Ok Committed | Ok Duplicate_event ->
       Metrics.incr ctx.metrics "cards_issued";
       push ctx card;
       ok (Views.card card)
     | Ok (Conflict _) ->
       (* A concurrent request with the same key won; answer with its card. *)
       let* card =
         Ctx.find ~what:"card" (Any_store.card_by_request ctx.store ~request_id)
       in
       ok (Views.card card)
     | Error e -> fail (Problem.internal e))
;;

let owned (ctx : Ctx.t) customer id =
  let* c =
    Ctx.find ~what:"card" (Any_store.card ctx.store (Card_core.Ids.Card_id.of_string id))
  in
  if Customer_id.equal c.customer customer then ok c else fail (Problem.not_found "card")
;;

let list (ctx : Ctx.t) customer =
  let* l = Ctx.read (Any_store.cards ctx.store customer) in
  ok (List.map l ~f:Views.card)
;;

let get ctx customer id =
  let* c = owned ctx customer id in
  ok (Views.card c)
;;

let update (ctx : Ctx.t) customer id ~action f =
  lock ctx id (fun () ->
    let* card = owned ctx customer id in
    let* info = Deferred.map (f card) ~f:Problem.of_provider in
    let card =
      { card with
        status = info.Banking_provider.Card_info.status
      ; limits = info.limits
      ; masked_pan = info.masked_pan
      ; updated_at = Ctx.now ctx
      }
    in
    let* () =
      Ctx.commit
        ctx
        [ Put_card card
        ; Ctx.audit
            ctx
            ~actor:("customer:" ^ Customer_id.to_string customer)
            ~action
            ~object_id:id
            ~outcome:"ok"
            ()
        ]
    in
    push ctx card;
    ok (Views.card card))
;;

let freeze ctx customer id =
  update ctx customer id ~action:"card.freeze" (fun card ->
    Bank.set_card_status ctx.bank card.id Frozen)
;;

let unfreeze ctx customer id =
  update ctx customer id ~action:"card.unfreeze" (fun card ->
    Bank.set_card_status ctx.bank card.id Active)
;;

let set_limits (ctx : Ctx.t) (s : Session.t) id (l : Api_types.Card.Limits.t) =
  let* () = Auth.require_fresh_sca ctx s in
  let* card = owned ctx s.customer id in
  let currency = card.limits.currency in
  let parse = function
    | None -> Ok None
    | Some a ->
      Or_error.bind (Payments.parse_amount a) ~f:(fun amount ->
        if Money.Currency.equal (Money.Amount.currency amount) currency
        then Ok (Some amount)
        else Or_error.error_string "limits are in the card's billing currency")
  in
  let* limits =
    return
      (Problem.of_validation
         (let open Or_error.Let_syntax in
          let%bind per_transaction = parse l.per_transaction in
          let%bind daily = parse l.daily in
          let%bind monthly = parse l.monthly in
          Card_core.Limits.create
            ?per_transaction
            ?daily
            ?monthly
            ~online:l.online
            ~contactless:l.contactless
            ~atm:l.atm
            currency))
  in
  update ctx s.customer id ~action:"card.limits" (fun card ->
    Bank.set_card_limits ctx.bank card.id limits)
;;

let pan_token (ctx : Ctx.t) (s : Session.t) id =
  let* () = Auth.require_fresh_sca ctx s in
  let* card = owned ctx s.customer id in
  let* answer =
    Deferred.map (Bank.card_details_token ctx.bank card.id) ~f:Problem.of_provider
  in
  let* () =
    Ctx.commit
      ctx
      [ Ctx.audit
          ctx
          ~actor:("customer:" ^ Customer_id.to_string s.customer)
          ~action:"card.pan_token"
          ~object_id:id
          ~outcome:
            (match answer with
             | Done _ -> "issued"
             | Challenge _ -> "provider_sca")
          ()
      ]
  in
  match answer with
  | Done token ->
    ok
      (`Token
        { Api_types.Card.Pan_token.token = Card_core.Pan_display_token.reveal_for_ui token
        ; expires_at = Views.time (Card_core.Pan_display_token.expires_at token)
        })
  | Challenge c ->
    Hashtbl.set
      ctx.provider_challenges
      ~key:(Payments_core.Sca.Id.to_string c.id)
      ~data:(s.customer, c.purpose);
    ok (`Challenge (Views.provider_challenge (Provider_challenge.of_provider c)))
;;

let display (ctx : Ctx.t) (s : Session.t) id =
  let* () = Auth.require_fresh_sca ctx s in
  let* card = owned ctx s.customer id in
  let* d = Deferred.map (Bank.card_display ctx.bank card.id) ~f:Problem.of_provider in
  let* () =
    Ctx.commit
      ctx
      [ Ctx.audit
          ctx
          ~actor:("customer:" ^ Customer_id.to_string s.customer)
          ~action:"card.display"
          ~object_id:id
          ~outcome:"ok"
          ()
      ]
  in
  ok
    { Api_types.Card.Display.provider = d.provider
    ; script_url = d.script_url
    ; application_id = d.application_id
    ; (* Meant for the browser component, which only drives the provider's iframe after
         the cardholder's OTP consent; this answer is where it is handed out. *)
      ui_secret = Option.map d.ui_secret ~f:Banking_provider.Sensitive.reveal
    ; user_id = d.user_id
    ; card_id = d.card_id
    }
;;

let consent (ctx : Ctx.t) (s : Session.t) id (r : Api_types.Card.Consent_request.t) =
  let* () = Auth.require_fresh_sca ctx s in
  let* card = owned ctx s.customer id in
  let* c =
    Deferred.map
      (Bank.card_consent_started ctx.bank card.id ~challenge_id:r.challenge_id)
      ~f:Problem.of_provider
  in
  Hashtbl.set
    ctx.provider_challenges
    ~key:(Payments_core.Sca.Id.to_string c.id)
    ~data:(s.customer, c.purpose);
  ok (Views.provider_challenge (Provider_challenge.of_provider c))
;;

let status_changed (ctx : Ctx.t) id status =
  match%bind Ctx.read (Any_store.card ctx.store id) with
  | Error p -> fail p
  | Ok None -> ok []
  | Ok (Some card) ->
    let card = { card with status; updated_at = Ctx.now ctx } in
    push ctx card;
    ok [ Store.Write.Put_card card ]
;;
