open! Core
open Async
open Harness

(* A restart keeps the database and the state directory; the fake bank's ids must not
   start over, or a new customer collides with the first at the provider. *)
let%expect_test "after a restart over the same state, a second customer signs up" =
  let%bind () =
    Pg.with_store (fun pg ->
      let store = Neobank_api.Any_store.pack (module Neobank_api.Store_pg) pg in
      let dir = Filename_unix.temp_dir "neobank-api" "restart" in
      let%bind () =
        with_api ~store ~state_dir:dir (fun t ->
          let%map (_ : string * string * string) = onboard t in
          ())
      in
      let%bind () =
        (* Our own ids are random in the second run, as in production: the counted ids of
           the first run are already in the database. *)
        with_api ~store ~state_dir:dir ~ids:(Neobank_api.Ids.random ()) (fun t ->
          let%bind (_ : string * string * string) =
            onboard ~email:"second@example.com" t
          in
          let%map r = get t "/me" in
          show "second customer" r ~fields:[ "kyc.status"; "accounts" ])
      in
      let%map (_ : Core_unix.Exit_or_signal.t) =
        In_thread.run (fun () ->
          Core_unix.system (sprintf "rm -rf %s" (Filename.quote dir)))
      in
      ())
  in
  [%expect {| second customer: 200 kyc.status=active accounts=6 |}];
  return ()
;;
