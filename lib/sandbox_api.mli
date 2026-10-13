(** The [/sandbox/*] routes: what a provider sandbox can simulate for the signed-in
    customer's own accounts, cards and transfers (a KYC decision, a deposit, a card
    purchase, a scheme's answer to a payment). Served only when the provider has a
    sandbox; never in production. *)

open! Core
open Neobank_api_types
open Model

val kyc_decision
  :  Ctx.t
  -> Customer_id.t
  -> Api_types.Sandbox.Kyc_decision.t
  -> Api_types.Kyc.Status.t Problem.res

(** Returns the deposit's transfer id. *)
val deposit : Ctx.t -> Customer_id.t -> Api_types.Sandbox.Deposit.t -> string Problem.res

val card_auth
  :  Ctx.t
  -> Customer_id.t
  -> Api_types.Sandbox.Card_auth.t
  -> Api_types.Sandbox.Card_auth_result.t Problem.res

val transfer_outcome
  :  Ctx.t
  -> Customer_id.t
  -> Api_types.Sandbox.Transfer_outcome.t
  -> Api_types.Transfer.t Problem.res
