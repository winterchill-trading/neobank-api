open! Core
open Async
open Model
module E = Banking_provider.Event

let ( let* ) = Deferred.Result.( >>= )
let ok = Deferred.Result.return

let customer_of_ref (ctx : Ctx.t) r =
  Ctx.read (Any_store.customer_by_provider_ref ctx.store r)
;;

let unmatched (ctx : Ctx.t) (e : E.t) =
  Metrics.incr ctx.metrics "events_unmatched" ~labels:[ "kind", E.Kind.name e.kind ];
  ok ([], ignore)
;;

let update_swap (ctx : Ctx.t) e swap ~f =
  match%bind Ctx.read (Any_store.swap ctx.store swap) with
  | Error p -> return (Error p)
  | Ok None -> unmatched ctx e
  | Ok (Some s) ->
    let s = { (f s) with Swap.updated_at = Ctx.now ctx } in
    ok
      ( [ Store.Write.Put_swap s ]
      , fun () -> Hub.publish ctx.hub s.customer (Swap (Views.swap s)) )
;;

(* The writes an event leads to, and what to push once they are committed. *)
let writes (ctx : Ctx.t) (e : E.t) : (Store.Write.t list * (unit -> unit)) Problem.res =
  match e.kind with
  | Ping -> ok ([], ignore)
  | Kyc { customer; _ } ->
    (match%bind customer_of_ref ctx customer with
     | Error p -> return (Error p)
     | Ok None -> unmatched ctx e
     | Ok (Some c) ->
       let* writes, view = Onboarding.writes_for_event ctx c e in
       ok
         ( writes
         , fun () ->
             Option.iter view ~f:(fun v -> Hub.publish ctx.hub c.id (Kyc (Views.kyc v)))
         ))
  | Account_opened a | Account_updated a ->
    (match%bind customer_of_ref ctx a.customer with
     | Error p -> return (Error p)
     | Ok None -> unmatched ctx e
     | Ok (Some c) ->
       let account =
         { Account.customer = c.id; provider = a; updated_at = Ctx.now ctx }
       in
       ok
         ( [ Store.Write.Put_account account ]
         , fun () -> Hub.publish ctx.hub c.id (Account (Views.account account)) ))
  | Incoming_transfer { account; transfer; _ } ->
    let* owner = Ctx.read (Any_store.account ctx.store account) in
    let* existing = Ctx.read (Any_store.transfer ctx.store transfer) in
    (match owner, existing with
     | None, _ -> unmatched ctx e
     | Some _, Some _ -> ok ([], ignore)
     | Some owner, None ->
       (match
          E.incoming_transfer
            e
            ~account:
              (Ledger.Account_id.of_string ("app:" ^ Account_ref.to_string account))
        with
        | None -> ok ([], ignore)
        | Some (Error err) -> return (Error (Problem.internal err))
        | Some (Ok x) ->
          let now = Ctx.now ctx in
          let record : Transfer.t =
            { transfer = x
            ; customer = owner.customer
            ; account
            ; beneficiary = None
            ; risk = None
            ; hold = Not_held
            ; challenge = None
            ; provider_challenge = None
            ; created_at = now
            ; updated_at = now
            }
          in
          ok
            ( [ Store.Write.Put_transfer record ]
            , fun () ->
                Hub.publish ctx.hub owner.customer (Transfer (Views.transfer record)) )))
  | Transfer { transfer; update; _ } ->
    (match%bind Ctx.read (Any_store.transfer ctx.store transfer) with
     | Error p -> return (Error p)
     | Ok None -> unmatched ctx e
     | Ok (Some record) ->
       (match Payments.apply_update ctx record update ~at:e.at with
        | Error err ->
          (* The state machine refused the report: a real inconsistency, kept for an
             operator rather than retried forever. *)
          Metrics.incr ctx.metrics "events_refused" ~labels:[ "kind", "transfer" ];
          Log.Global.error_s
            [%message
              "transfer report refused"
                ~transfer:(Payments_core.Transfer.Id.to_string transfer)
                (err : Error.t)];
          ok ([], ignore)
        | Ok None -> ok ([], ignore)
        | Ok (Some record) ->
          ok
            ( [ Store.Write.Put_transfer record ]
            , fun () ->
                Hub.publish ctx.hub record.customer (Transfer (Views.transfer record)) )))
  | Payee_check { beneficiary; result } ->
    (match%bind Ctx.read (Any_store.beneficiary ctx.store beneficiary) with
     | Error p -> return (Error p)
     | Ok None -> unmatched ctx e
     | Ok (Some b) ->
       ok
         ( [ Store.Write.Put_beneficiary
               (Payments_core.Beneficiary.with_payee_check b result)
           ]
         , ignore ))
  | Swap_executed { swap; sell; fee; buy } ->
    update_swap ctx e swap ~f:(fun s ->
      { s with status = Executed; sell; fee; buy; provider_challenge = None })
  | Swap_failed { swap; reason } ->
    update_swap ctx e swap ~f:(fun s ->
      { s with status = Failed reason; provider_challenge = None })
  | Card { card; update } ->
    let message =
      match update with
      | Status_changed _ -> "status"
      | Auth_request _ -> "auth_request"
      | Auth_advice _ -> "auth_advice"
      | Reversal _ -> "reversal"
      | Clearing _ -> "clearing"
      | Expiry _ -> "expiry"
    in
    Metrics.incr ctx.metrics "card_messages" ~labels:[ "kind", message ];
    (match update with
     | Status_changed status ->
       let* w = Cards.status_changed ctx card status in
       ok (w, ignore)
     | Auth_request _ | Auth_advice _ | Reversal _ | Clearing _ | Expiry _ ->
       (* Card money movements are the provider's books; balances are read from it. *)
       ok ([], ignore))
  | Standing_order_run { order; run_date; transfer } ->
    let* w = Standing_orders.run_writes ctx order ~run_date ~transfer in
    ok (w, ignore)
;;

let lock_key (e : E.t) =
  match e.kind with
  | Transfer { transfer; _ } -> "transfer:" ^ Payments_core.Transfer.Id.to_string transfer
  | Swap_executed { swap; _ } | Swap_failed { swap; _ } ->
    "swap:" ^ Payments_core.Swap.Id.to_string swap
  | _ -> "events"
;;

let sequencer = Throttle.Sequencer.create ()

let apply (ctx : Ctx.t) (e : E.t) =
  Throttle.enqueue sequencer (fun () ->
    Keyed_lock.with_ ctx.locks ~key:(lock_key e) (fun () ->
      let kind = E.Kind.name e.kind in
      match%bind Any_store.inbox_mem ctx.store e.id with
      | Error _ as err -> return err
      | Ok true ->
        Metrics.incr ctx.metrics "events_duplicate" ~labels:[ "kind", kind ];
        return (Ok `Duplicate)
      | Ok false ->
        (match%bind writes ctx e with
         | Error p ->
           return (Or_error.error_s [%message "event not applied" kind (p : Problem.t)])
         | Ok (writes, after) ->
           let inbox = Model.Inbox_entry.of_event e ~received_at:(Ctx.now ctx) in
           (match%map Any_store.commit ctx.store (Store.Write.Inbox inbox :: writes) with
            | Ok Committed ->
              Metrics.incr ctx.metrics "events_applied" ~labels:[ "kind", kind ];
              after ();
              Ok `Applied
            | Ok Duplicate_event ->
              Metrics.incr ctx.metrics "events_duplicate" ~labels:[ "kind", kind ];
              Ok `Duplicate
            | Ok (Conflict c) -> Or_error.error_s [%message "event conflicts" kind c]
            | Error _ as err -> err))))
;;

let ingest_webhook (ctx : Ctx.t) request =
  match%bind Bank.ingest_webhook ctx.bank request with
  | Error _ ->
    Metrics.incr ctx.metrics "webhooks_refused";
    return
      (Error
         (Problem.unauthorized
            "bad_signature"
            "the delivery is not signed by the provider"))
  | Ok events ->
    let%bind results = Deferred.List.map ~how:`Sequential events ~f:(apply ctx) in
    let applied =
      List.count results ~f:(function
        | Ok `Applied -> true
        | _ -> false)
    in
    let duplicates =
      List.count results ~f:(function
        | Ok `Duplicate -> true
        | _ -> false)
    in
    (match
       List.find_map results ~f:(function
         | Error e -> Some e
         | Ok _ -> None)
     with
     | Some e ->
       Log.Global.error_s [%message "webhook event failed" (e : Error.t)];
       (* A 5xx makes the provider redeliver; the inbox skips what already applied. *)
       return (Error (Problem.internal e))
     | None ->
       let%bind () = Ctx.settle ctx in
       ok (applied, duplicates))
;;

let drain () =
  (* The consumer below reads the provider's pipe in its own Async jobs; once no job is
     left, every event published so far is queued on the sequencer, behind which this
     waits. *)
  let%bind () = Scheduler.yield_until_no_jobs_remain () in
  Throttle.enqueue sequencer (fun () -> return ())
;;

let start (ctx : Ctx.t) =
  ctx.drain <- drain;
  let events = Bank.events ctx.bank in
  don't_wait_for
    (Pipe.iter events ~f:(fun e ->
       match%map apply ctx e with
       | Ok (`Applied | `Duplicate) -> don't_wait_for (ctx.kick ())
       | Error err ->
         Log.Global.error_s [%message "provider event failed" (err : Error.t)]));
  fun () -> Pipe.close_read events
;;
