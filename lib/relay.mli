(** The outbox relay: sends each queued provider call at least once.

    A message is written in the same transaction as the state change it announces, so it
    exists if and only if that change committed. The relay polls every second (and runs at
    once when a request {!Ctx.kick}s it), calls the provider, and marks the message sent.
    A failure is retried with a backoff from 1 s doubling to 5 minutes; after 24 hours the
    message is parked as [Needs_review] for an operator. A crash between the provider call
    and the mark resends the message, which is safe: every call is keyed by our id
    (customer, transfer, swap, standing order) and the handlers skip work that is already
    done.

    Each pass also brings the ledger projection up to date ({!Ledger_projection}). The
    relay prunes idempotency keys (24 h), inbox rows (30 days) and idle sessions, hourly. *)

open! Core
open Async

(** One pass over the due messages ({!Ctx.kick}); concurrent calls share a pass. Does
    nothing before {!start}. *)
val run_once : Ctx.t -> unit Deferred.t

(** Starts the periodic passes and pruning, and points {!Ctx.kick} at {!run_once}. Stops
    when [stop] is determined. *)
val start : Ctx.t -> stop:unit Deferred.t -> unit

(** [min (1 s * 2 ^ (attempts - 1)) max]. *)
val backoff : attempts:int -> max:Time_ns.Span.t -> Time_ns.Span.t
