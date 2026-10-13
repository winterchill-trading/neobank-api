open! Core
open Async
open Neobank_api_types
open Model
module P = Payments_core

let ( let* ) = Deferred.Result.( >>= )
let ok = Deferred.Result.return
let fail p = return (Error p)
let validation r = return (Problem.of_validation r)
let lock (ctx : Ctx.t) id f = Keyed_lock.with_ ctx.locks ~key:("standing-order:" ^ id) f

let push (ctx : Ctx.t) (o : Standing_order.t) =
  Hub.publish ctx.hub o.customer (Standing_order (Views.standing_order o))
;;

let create (ctx : Ctx.t) (s : Session.t) (r : Api_types.Standing_order.Request.t) =
  let* () = Auth.require_fresh_sca ctx s in
  let* account = Accounts.owned ctx s.customer r.account_id in
  let* beneficiary = Payments.owned_beneficiary ctx s.customer r.beneficiary_id in
  let* amount = validation (Payments.parse_amount r.amount) in
  let* schedule = validation (Views.schedule_of_string r.schedule) in
  let date s =
    Or_error.try_with (fun () -> Date.of_string s)
    |> Or_error.tag ~tag:"dates are YYYY-MM-DD"
  in
  let* start = validation (date r.start) in
  let* until =
    match r.until with
    | None -> ok None
    | Some u -> validation (Or_error.map (date u) ~f:Option.some)
  in
  let* rail = validation (Payments.pick_rail ~account ~beneficiary None) in
  let* payer = Payments.customer_name ctx s.customer in
  let id = Ids.next ctx.ids "so" in
  let reference = Option.value r.reference ~default:"" in
  let* details =
    validation
      (Payments.details
         ctx
         ~id
         ~rail
         ~account
         ~beneficiary
         ~payer
         ~wallet_provider:None
         ~reference)
  in
  let* order =
    validation
      (P.Standing_order.create
         ~id:(P.Standing_order.Id.of_string id)
         ~account:
           (Ledger.Account_id.of_string
              ("app:" ^ Account_ref.to_string (Account.id account)))
         ~beneficiary
         ~rail
         ~amount
         ~reference
         ?details
         ~schedule
         ~start
         ?until
         ())
  in
  let now = Ctx.now ctx in
  let record : Standing_order.t =
    { order
    ; customer = s.customer
    ; account = Account.id account
    ; provider_ref = None
    ; provider_challenge = None
    ; created_at = now
    ; updated_at = now
    }
  in
  let* () =
    Ctx.commit
      ctx
      [ Put_standing_order record
      ; Enqueue
          (Outbox.create
             ~id:(Ids.next ctx.ids "msg")
             ~now
             (Create_standing_order order.id))
      ; Ctx.audit
          ctx
          ~actor:("customer:" ^ Customer_id.to_string s.customer)
          ~action:"standing_order.create"
          ~object_id:id
          ~outcome:"queued"
          ()
      ]
  in
  let%bind () = Ctx.settle ctx in
  let* record =
    Ctx.find ~what:"standing order" (Any_store.standing_order ctx.store order.id)
  in
  ok (Views.standing_order record)
;;

let list (ctx : Ctx.t) customer =
  let* l = Ctx.read (Any_store.standing_orders ctx.store customer) in
  ok (List.map l ~f:Views.standing_order)
;;

let cancel (ctx : Ctx.t) customer id =
  lock ctx id (fun () ->
    let* o =
      Ctx.find
        ~what:"standing order"
        (Any_store.standing_order ctx.store (P.Standing_order.Id.of_string id))
    in
    let* () =
      if Customer_id.equal o.customer customer
      then ok ()
      else fail (Problem.not_found "standing order")
    in
    let* () =
      match o.provider_ref with
      | None -> ok ()
      | Some r ->
        Deferred.map (Bank.cancel_standing_order ctx.bank r) ~f:Problem.of_provider
    in
    let o =
      { o with
        order = P.Standing_order.cancel o.order
      ; provider_challenge = None
      ; updated_at = Ctx.now ctx
      }
    in
    let* () =
      Ctx.commit
        ctx
        [ Put_standing_order o
        ; Ctx.audit
            ctx
            ~actor:("customer:" ^ Customer_id.to_string customer)
            ~action:"standing_order.cancel"
            ~object_id:id
            ~outcome:"ok"
            ()
        ]
    in
    push ctx o;
    ok (Views.standing_order o))
;;

let send (ctx : Ctx.t) id =
  let key = P.Standing_order.Id.to_string id in
  lock ctx key (fun () ->
    let open Deferred.Or_error.Let_syntax in
    match%bind Any_store.standing_order ctx.store id with
    | None -> Deferred.Or_error.error_s [%message "no such standing order" key]
    | Some
        ({ provider_ref = None
         ; provider_challenge = None
         ; order = { status = Active; _ }
         ; _
         } as o) ->
      let%bind answer = Bank.create_standing_order ctx.bank ~from:o.account o.order in
      let now = Ctx.now ctx in
      let o =
        match answer with
        | Done r -> { o with provider_ref = Some r; updated_at = now }
        | Challenge c ->
          let provider_ref =
            match c.purpose with
            | Standing_order r -> Some r
            | Transfer _ | Swap _ | Card_details _ -> None
          in
          Hashtbl.set
            ctx.provider_challenges
            ~key:(P.Sca.Id.to_string c.id)
            ~data:(o.customer, c.purpose);
          { o with
            provider_challenge = Some (Provider_challenge.of_provider c)
          ; provider_ref
          ; updated_at = now
          }
      in
      let%map (_ : Store.Commit_result.t) =
        Any_store.commit ctx.store [ Put_standing_order o ]
      in
      push ctx o
    | Some _ -> return ())
;;

let challenge_accepted (ctx : Ctx.t) r =
  match%bind Ctx.read (Any_store.standing_order_by_provider_ref ctx.store r) with
  | Error p -> fail p
  | Ok None -> ok ()
  | Ok (Some o) ->
    lock
      ctx
      (P.Standing_order.Id.to_string (Standing_order.id o))
      (fun () ->
        let o = { o with provider_challenge = None; updated_at = Ctx.now ctx } in
        let* () = Ctx.commit ctx [ Put_standing_order o ] in
        push ctx o;
        ok ())
;;

let run_writes (ctx : Ctx.t) r ~run_date ~transfer =
  match%bind Ctx.read (Any_store.standing_order_by_provider_ref ctx.store r) with
  | Error p -> fail p
  | Ok None -> ok []
  | Ok (Some o) ->
    let now = Ctx.now ctx in
    let x = o.order in
    let order =
      { x with
        runs = x.runs + 1
      ; last_run = Some run_date
      ; next_run = P.Schedule.next_after x.schedule run_date
      }
    in
    let o = { o with order; updated_at = now } in
    (* The run's payment, so its status reports have a transfer to land on; recurring runs
       after the first are exempt from SCA (RTS art. 14). *)
    let* draft =
      return
        (Problem.of_internal
           (let open Or_error.Let_syntax in
            let%bind draft =
              P.Transfer.draft
                ~id:transfer
                ~rail:x.rail
                ~account:x.account
                ~beneficiary:x.beneficiary
                ~amount:x.amount
                ~reference:x.reference
                ?details:x.details
                ?eur_value:(Ctx.eur_value ctx x.amount)
                ~now
                ()
            in
            let%bind draft =
              P.Transfer.exempt draft ~exemption:Recurring_same_amount_payee
            in
            P.Transfer.submit draft ~at:now ()))
    in
    let record : Transfer.t =
      { transfer = draft
      ; customer = o.customer
      ; account = o.account
      ; beneficiary = Some x.beneficiary.id
      ; risk = None
      ; hold = Not_held
      ; challenge = None
      ; provider_challenge = None
      ; created_at = now
      ; updated_at = now
      }
    in
    push ctx o;
    Hub.publish ctx.hub o.customer (Transfer (Views.transfer record));
    let* existing = Ctx.read (Any_store.transfer ctx.store transfer) in
    ok
      (Store.Write.Put_standing_order o
       :: (if Option.is_some existing then [] else [ Store.Write.Put_transfer record ]))
;;
