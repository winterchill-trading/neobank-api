(** Any {!Store.S}, chosen at startup: the service is written once against this and runs
    on {!Store_memory} or {!Store_pg}. *)

open! Core

type t

val pack : (module Store.S with type t = 'a) -> 'a -> t

(** The packed store's {!Store.S.kind}. *)
val kind_of : t -> string

include Store.S with type t := t
