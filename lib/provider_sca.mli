(** [POST /sca/:id]: answers a provider's own challenge (its SMS or app code) for a swap,
    a standing order or revealing a card number, then lets the operation continue.
    Transfers have their own route ([/transfers/:id/confirm-sca]) but are accepted here
    too. Only the customer the challenge was issued to may answer it. *)

open! Core
open Neobank_api_types
open Model

val confirm
  :  Ctx.t
  -> Customer_id.t
  -> string
  -> Api_types.Sca_confirm_request.t
  -> [ `Accepted ] Problem.res
