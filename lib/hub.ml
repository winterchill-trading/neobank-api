open! Core
open Async
module Ws_push = Neobank_api_types.Api_types.Ws_push

type t = Ws_push.t Pipe.Writer.t list String.Table.t

let create () = String.Table.create ()
let key = Kyc_core.Customer_id.to_string

let subscribe t customer =
  let reader, writer = Pipe.create () in
  Hashtbl.add_multi t ~key:(key customer) ~data:writer;
  don't_wait_for
    (let%map () = Pipe.closed writer in
     Hashtbl.change t (key customer) ~f:(function
       | None -> None
       | Some ws ->
         (match List.filter ws ~f:(fun w -> not (phys_equal w writer)) with
          | [] -> None
          | ws -> Some ws)));
  reader
;;

let publish t customer push =
  List.iter
    (Hashtbl.find_multi t (key customer))
    ~f:(fun w -> if not (Pipe.is_closed w) then Pipe.write_without_pushback w push)
;;

let connections t =
  Hashtbl.fold t ~init:0 ~f:(fun ~key:_ ~data acc -> acc + List.length data)
;;

let close_all t =
  Hashtbl.iter t ~f:(List.iter ~f:Pipe.close);
  Hashtbl.clear t
;;
