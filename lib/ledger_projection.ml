open! Core
open Async

let batch = 200

let catch_up (ctx : Ctx.t) =
  match Bank.journal ctx.bank with
  | None -> return (Ok 0)
  | Some journal ->
    let open Deferred.Or_error.Let_syntax in
    let%bind head = Any_store.ledger_head ctx.store in
    let pending =
      List.filter (Ledger.Journal.records journal) ~f:(fun (r : Ledger.Record.t) ->
        r.seq > head)
    in
    let%map () =
      Deferred.Or_error.List.iter
        ~how:`Sequential
        (List.chunks_of pending ~length:batch)
        ~f:(fun chunk ->
          let%bind result =
            Any_store.commit
              ctx.store
              (List.map chunk ~f:(fun r -> Store.Write.Ledger_record r))
          in
          match result with
          | Committed | Duplicate_event -> return ()
          | Conflict c ->
            Deferred.Or_error.error_s [%message "ledger projection conflict" c])
    in
    let n = List.length pending in
    if n > 0 then Metrics.incr ctx.metrics "ledger_records_projected" ~by:n;
    n
;;
