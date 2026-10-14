open! Core
open Async
open Neobank_api
open Harness

let%expect_test "a failing outbox message backs off, then waits for an operator" =
  let%bind () =
    with_api (fun t ->
      let now () = Async_kernel.Time_source.now t.clock in
      let message =
        Model.Outbox.create
          ~id:"msg-test"
          ~now:(now ())
          (Initiate_transfer (Payments_core.Transfer.Id.of_string "tr-missing"))
      in
      let%bind () =
        Ctx.commit t.ctx [ Enqueue message ]
        >>| Result.map_error ~f:(fun p -> Error.create_s [%sexp (p : Problem.t)])
        >>| ok_exn
      in
      let show_message label =
        let%map m = Any_store.outbox t.ctx.store "msg-test" >>| ok_exn in
        let m = Option.value_exn m in
        print_s
          [%message
            label
              ~state:(Model.Outbox.State.name m.state : string)
              ~attempts:(m.attempts : int)
              ~retry_in:(Time_ns.diff m.next_attempt_at (now ()) : Time_ns.Span.t)
              ~last_error:(m.last_error : string option)]
      in
      let%bind () = Relay.run_once t.ctx in
      let%bind () = show_message "first try" in
      (* Not due yet: nothing happens. *)
      let%bind () = Relay.run_once t.ctx in
      let%bind () = show_message "too early" in
      advance t (Time_ns.Span.of_sec 1.);
      let%bind () = Relay.run_once t.ctx in
      let%bind () = show_message "second try" in
      advance t (Time_ns.Span.of_day 1.);
      let%bind () = Relay.run_once t.ctx in
      let%bind () = show_message "a day later" in
      let%bind r = get t "/ops/outbox" ~token:`Ops in
      show "ops" r ~fields:[ "0.id"; "0.kind"; "0.state" ];
      return ())
  in
  [%expect
    {|
    ("first try" (state pending) (attempts 1) (retry_in 1s)
     (last_error ("(\"no such transfer\" tr-missing)")))
    ("too early" (state pending) (attempts 1) (retry_in 1s)
     (last_error ("(\"no such transfer\" tr-missing)")))
    ("second try" (state pending) (attempts 2) (retry_in 2s)
     (last_error ("(\"no such transfer\" tr-missing)")))
    ("a day later" (state needs_review) (attempts 3) (retry_in -23h59m58s)
     (last_error ("(\"no such transfer\" tr-missing)")))
    ops: 200 0.id=msg-test 0.kind=initiate_transfer 0.state=needs_review
    |}];
  return ()
;;
