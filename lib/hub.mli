(** Fans pushes out to each customer's open [/ws] connections. *)

open! Core
open Async

type t

val create : unit -> t

(** Registers a connection; the returned reader yields the customer's pushes until it is
    closed. *)
val subscribe
  :  t
  -> Kyc_core.Customer_id.t
  -> Neobank_api_types.Api_types.Ws_push.t Pipe.Reader.t

val publish : t -> Kyc_core.Customer_id.t -> Neobank_api_types.Api_types.Ws_push.t -> unit
val connections : t -> int
val close_all : t -> unit
