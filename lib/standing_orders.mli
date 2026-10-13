(** Recurring payments. Creating one needs a fresh SCA and goes through the outbox to the
    provider, which may ask for its own code once ([POST /sca/:id]); later runs are exempt
    from SCA (RTS art. 14) and arrive as [Standing_order_run] events, each with its
    transfer. *)

open! Core
open Async
open Neobank_api_types
open Model

val create
  :  Ctx.t
  -> Session.t
  -> Api_types.Standing_order.Request.t
  -> Api_types.Standing_order.t Problem.res

val list : Ctx.t -> Customer_id.t -> Api_types.Standing_order.t list Problem.res
val cancel : Ctx.t -> Customer_id.t -> string -> Api_types.Standing_order.t Problem.res

(** Outbox work. *)
val send : Ctx.t -> Payments_core.Standing_order.Id.t -> unit Or_error.t Deferred.t

(** After the provider accepted its code for the order. *)
val challenge_accepted
  :  Ctx.t
  -> Banking_provider.Standing_order_ref.t
  -> unit Problem.res

(** The writes for one run: the order's counters and the run's transfer, which the
    transfer events then move along. *)
val run_writes
  :  Ctx.t
  -> Banking_provider.Standing_order_ref.t
  -> run_date:Date.t
  -> transfer:Payments_core.Transfer.Id.t
  -> Store.Write.t list Problem.res
