open! Core
open Async
open Neobank_api_types
open Model
module P = Payments_core

let ( let* ) = Deferred.Result.( >>= )
let ok = Deferred.Result.return
let fail p = return (Error p)
let push (ctx : Ctx.t) (s : Swap.t) = Hub.publish ctx.hub s.customer (Swap (Views.swap s))
let lock (ctx : Ctx.t) id f = Keyed_lock.with_ ctx.locks ~key:("swap:" ^ id) f

let quote (ctx : Ctx.t) (s : Session.t) (r : Api_types.Swap.Quote_request.t) =
  let* from = Accounts.owned ctx s.customer r.from_account_id in
  let* to_ = Accounts.owned ctx s.customer r.to_account_id in
  let* sell = return (Problem.of_validation (Payments.parse_amount r.sell)) in
  let* () =
    if Money.Currency.equal (Money.Amount.currency sell) (Account.currency from)
    then ok ()
    else
      fail
        (Problem.bad_request
           "currency_mismatch"
           "sell must be in the source account's currency")
  in
  let* q =
    Deferred.map
      (Bank.quote_swap ctx.bank ~from:(Account.id from) ~to_:(Account.id to_) ~sell)
      ~f:Problem.of_provider
  in
  let now = Ctx.now ctx in
  let swap : Swap.t =
    { id = q.id
    ; customer = s.customer
    ; from_account = Account.id from
    ; to_account = Account.id to_
    ; sell = q.sell
    ; fee = q.fee
    ; buy = q.buy
    ; price = Money.Rate.price_to_string (P.Swap.Quote.effective_price q)
    ; expires_at = q.expires_at
    ; status = Quoted
    ; provider_challenge = None
    ; created_at = now
    ; updated_at = now
    }
  in
  let* () = Ctx.commit ctx [ Put_swap swap ] in
  ok (Views.swap swap)
;;

let owned (ctx : Ctx.t) customer id =
  let* s = Ctx.find ~what:"swap" (Any_store.swap ctx.store (P.Swap.Id.of_string id)) in
  if Customer_id.equal s.customer customer then ok s else fail (Problem.not_found "swap")
;;

let execute (ctx : Ctx.t) (session : Session.t) id =
  let* () = Auth.require_fresh_sca ctx session in
  let* swap =
    lock ctx id (fun () ->
      let* swap = owned ctx session.customer id in
      let now = Ctx.now ctx in
      match swap.status with
      | Quoted when Time_ns.( > ) now swap.expires_at ->
        fail (Problem.conflict "quote_expired" "the quote expired; ask for a new one")
      | Quoted ->
        let swap = { swap with status = Executing; updated_at = now } in
        let* () =
          Ctx.commit
            ctx
            [ Put_swap swap
            ; Enqueue
                (Outbox.create ~id:(Ids.next ctx.ids "msg") ~now (Execute_swap swap.id))
            ; Ctx.audit
                ctx
                ~actor:("customer:" ^ Customer_id.to_string session.customer)
                ~action:"swap.execute"
                ~object_id:id
                ~outcome:"queued"
                ()
            ]
        in
        ok swap
      | Executing | Awaiting_sca | Executed | Failed _ -> ok swap)
  in
  let%bind () = ctx.kick () in
  let* swap = owned ctx session.customer (P.Swap.Id.to_string swap.id) in
  ok (Views.swap swap)
;;

let get ctx customer id =
  let* s = owned ctx customer id in
  ok (Views.swap s)
;;

let list (ctx : Ctx.t) customer =
  let* l = Ctx.read (Any_store.swaps ctx.store customer) in
  ok (List.map l ~f:Views.swap)
;;

let send (ctx : Ctx.t) id =
  let key = P.Swap.Id.to_string id in
  lock ctx key (fun () ->
    let open Deferred.Or_error.Let_syntax in
    match%bind Any_store.swap ctx.store id with
    | None -> Deferred.Or_error.error_s [%message "no such swap" key]
    | Some ({ status = Executing; provider_challenge = None; _ } as swap) ->
      let%bind answer = Bank.execute_swap ctx.bank id in
      let swap =
        match answer with
        | Done () -> swap
        | Challenge c ->
          Hashtbl.set
            ctx.provider_challenges
            ~key:(P.Sca.Id.to_string c.id)
            ~data:(swap.customer, c.purpose);
          { swap with
            status = Awaiting_sca
          ; provider_challenge = Some (Provider_challenge.of_provider c)
          ; updated_at = Ctx.now ctx
          }
      in
      let%map (_ : Store.Commit_result.t) =
        Any_store.commit ctx.store [ Put_swap swap ]
      in
      push ctx swap
    | Some _ -> return ())
;;

let challenge_accepted (ctx : Ctx.t) id =
  let key = P.Swap.Id.to_string id in
  lock ctx key (fun () ->
    let* swap = Ctx.find ~what:"swap" (Any_store.swap ctx.store id) in
    match swap.status with
    | Awaiting_sca ->
      let swap =
        { swap with
          status = Executing
        ; provider_challenge = None
        ; updated_at = Ctx.now ctx
        }
      in
      let* () = Ctx.commit ctx [ Put_swap swap ] in
      push ctx swap;
      ok ()
    | _ -> ok ())
;;
