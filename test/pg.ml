(* A fresh database on pg-async's shared throwaway cluster, as a URL for Store_pg.connect. *)

open! Core
open Async

let fresh_database_url () =
  let%bind cluster = Pg_test.shared () in
  let%map config = Pg_test.create_database cluster in
  sprintf
    "postgres://postgres@127.0.0.1:%d/%s?sslmode=disable"
    (Pg_test.port cluster)
    (Option.value_exn config.dbname)
;;

let with_store f =
  let%bind url = fresh_database_url () in
  let%bind store = Neobank_api.Store_pg.connect ~url () >>| ok_exn in
  Monitor.protect
    (fun () -> f store)
    ~finally:(fun () -> Neobank_api.Store_pg.close store)
;;
