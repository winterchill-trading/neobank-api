(** Fresh identifiers for our own objects: [<prefix>-<16 hex digits>] from the system's
    secure random generator, or [<prefix>-000001], [<prefix>-000002], ... from a counter
    in tests, so expect output is stable.

    Ids use [-] rather than [_] because SEPA end-to-end ids (which are our transfer ids)
    only allow [a-z A-Z 0-9 / - ? : ( ) . , ' +] and space. *)

open! Core

type t

val random : unit -> t
val counter : unit -> t
val next : t -> string -> string

(** 32 random bytes as hex: session tokens. *)
val token : t -> string
