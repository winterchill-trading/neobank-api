(** Currency swaps at the provider's quoted price. A quote fixes what the customer gives
    and gets for a short time; executing it needs a fresh SCA and goes through the outbox;
    the provider may ask for its own code ([POST /sca/:id]); the result arrives as a
    [Swap_executed] or [Swap_failed] event. *)

open! Core
open Async
open Neobank_api_types
open Model

val quote
  :  Ctx.t
  -> Session.t
  -> Api_types.Swap.Quote_request.t
  -> Api_types.Swap.t Problem.res

val execute : Ctx.t -> Session.t -> string -> Api_types.Swap.t Problem.res
val get : Ctx.t -> Customer_id.t -> string -> Api_types.Swap.t Problem.res
val list : Ctx.t -> Customer_id.t -> Api_types.Swap.t list Problem.res

(** Outbox work. *)
val send : Ctx.t -> Payments_core.Swap.Id.t -> unit Or_error.t Deferred.t

(** After the provider accepted its code. *)
val challenge_accepted : Ctx.t -> Payments_core.Swap.Id.t -> unit Problem.res
