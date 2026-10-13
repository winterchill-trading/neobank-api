(** Serializes work per key (a transfer, a swap, a customer) inside one process, so a
    webhook, the outbox relay and a request never read-modify-write the same record at
    once. Different keys run concurrently. Locks are dropped when idle. *)

open! Core
open Async

type t

val create : unit -> t
val with_ : t -> key:string -> (unit -> 'a Deferred.t) -> 'a Deferred.t
