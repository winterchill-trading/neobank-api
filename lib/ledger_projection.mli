(** Copies the ledger journal into the store's ledger tables, so operators can query
    postings and the trial balance with SQL while the journal file stays the system of
    record. Projection is by sequence number and idempotent: it resumes after the last
    record projected, and a record projected twice is skipped. *)

open! Core
open Async

(** Projects records after the store's head; the number projected. Nothing when the bank
    keeps no journal. *)
val catch_up : Ctx.t -> int Or_error.t Deferred.t
