open! Core
open Async

type entry =
  { sequencer : unit Throttle.Sequencer.t
  ; mutable users : int
  }

type t = entry String.Table.t

let create () = String.Table.create ()

let with_ t ~key f =
  let entry =
    Hashtbl.find_or_add t key ~default:(fun () ->
      { sequencer = Throttle.Sequencer.create (); users = 0 })
  in
  entry.users <- entry.users + 1;
  Monitor.protect
    (fun () -> Throttle.enqueue entry.sequencer (fun () -> f ()))
    ~finally:(fun () ->
      entry.users <- entry.users - 1;
      if entry.users = 0 then Hashtbl.remove t key;
      return ())
;;
