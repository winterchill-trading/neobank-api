(** {!Store.S} in memory: for tests, the offline demo and the web app's mock mode. Same
    rules as {!Store_pg} (the conformance suite runs over both); nothing survives the
    process. *)

open! Core

type t

val create : unit -> t

include Store.S with type t := t
