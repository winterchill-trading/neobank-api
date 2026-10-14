open! Core
open Async
open Model

let backoff ~attempts ~max =
  let doubling = Float.( ** ) 2. (Float.of_int (Int.max 0 (attempts - 1))) in
  Time_ns.Span.min max (Time_ns.Span.scale (Time_ns.Span.of_sec 1.) doubling)
;;

let handle (ctx : Ctx.t) (kind : Outbox.Kind.t) =
  match kind with
  | Create_customer id -> Onboarding.create_provider_customer ctx id
  | Open_accounts id -> Onboarding.open_accounts ctx id
  | Initiate_transfer id -> Payments.initiate ctx id
  | Execute_swap id -> Swaps.send ctx id
  | Create_standing_order id -> Standing_orders.send ctx id
;;

let process (ctx : Ctx.t) (m : Outbox.t) =
  let%bind result =
    Monitor.try_with_or_error ~rest:`Log (fun () -> handle ctx m.kind) >>| Or_error.join
  in
  let now = Ctx.now ctx in
  let attempts = m.attempts + 1 in
  let kind = Outbox.Kind.name m.kind in
  let m : Outbox.t =
    match result with
    | Ok () ->
      Metrics.incr ctx.metrics "outbox_sent" ~labels:[ "kind", kind ];
      { m with attempts; state = Sent { at = now }; last_error = None }
    | Error e ->
      Metrics.incr ctx.metrics "outbox_failures" ~labels:[ "kind", kind ];
      Log.Global.info_s
        [%message "outbox message failed" m.id kind (attempts : int) (e : Error.t)];
      if Time_ns.Span.( >= )
           (Time_ns.diff now m.created_at)
           ctx.config.relay_give_up_after
      then (
        Metrics.incr ctx.metrics "outbox_needs_review" ~labels:[ "kind", kind ];
        { m with
          attempts
        ; state = Needs_review
        ; last_error = Some (Error.to_string_hum e)
        })
      else
        { m with
          attempts
        ; next_attempt_at =
            Time_ns.add now (backoff ~attempts ~max:ctx.config.relay_backoff_max)
        ; last_error = Some (Error.to_string_hum e)
        }
  in
  match%map Any_store.commit ctx.store [ Update_outbox m ] with
  | Ok _ -> ()
  | Error e -> Log.Global.error_s [%message "outbox update failed" m.id (e : Error.t)]
;;

let pass (ctx : Ctx.t) =
  match%bind Any_store.outbox_due ctx.store ~now:(Ctx.now ctx) ~limit:100 with
  | Error e ->
    Log.Global.error_s [%message "outbox read failed" (e : Error.t)];
    return ()
  | Ok messages ->
    let%bind () = Deferred.List.iter ~how:`Sequential messages ~f:(process ctx) in
    (match%map Ledger_projection.catch_up ctx with
     | Ok (_ : int) -> ()
     | Error e -> Log.Global.error_s [%message "ledger projection failed" (e : Error.t)])
;;

let prune (ctx : Ctx.t) =
  let now = Ctx.now ctx in
  match%map
    Any_store.prune
      ctx.store
      ~idempotency_before:(Time_ns.sub now ctx.config.idempotency_ttl)
      ~inbox_before:(Time_ns.sub now ctx.config.inbox_ttl)
      ~sessions_before:(Time_ns.sub now ctx.config.session_max)
  with
  | Ok pruned -> Log.Global.info_s [%message "pruned" (pruned : Store.Pruned.t)]
  | Error e -> Log.Global.error_s [%message "pruning failed" (e : Error.t)]
;;

let run_once (ctx : Ctx.t) = ctx.kick ()

let start (ctx : Ctx.t) ~stop =
  (* One pass at a time. A caller arriving while a pass runs waits for the next one, which
     sees its message; callers arriving together share that next pass. *)
  let sequencer = Throttle.Sequencer.create () in
  let queued = ref None in
  let kick () =
    match !queued with
    | Some next -> next
    | None ->
      let next =
        Throttle.enqueue sequencer (fun () ->
          queued := None;
          pass ctx)
      in
      queued := Some next;
      next
  in
  ctx.kick <- kick;
  Time_source.every
    ~stop
    ~continue_on_error:true
    ctx.time_source
    ctx.config.relay_interval
    (fun () -> don't_wait_for (kick ()));
  Time_source.every
    ~start:(Time_source.after ctx.time_source (Time_ns.Span.of_min 1.))
    ~stop
    ~continue_on_error:true
    ctx.time_source
    (Time_ns.Span.of_hr 1.)
    (fun () -> don't_wait_for (prune ctx))
;;
