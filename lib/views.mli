(** Domain values as API bodies ({!Neobank_api_types.Api_types}). The one place that
    decides what a customer sees: risk reasons never leave here (a held payment says
    "under review"), card numbers are masked, and no secret is rendered. *)

open! Core
open Neobank_api_types
open Model

(** ISO 8601 UTC with milliseconds, e.g. [2026-10-14T09:00:00.000Z]. *)
val time : Time_ns.t -> string

val amount : Money.Amount.t -> Api_types.Amount.t
val sca_challenge : Payments_core.Sca.Challenge.t -> Api_types.Sca_info.t
val provider_challenge : Provider_challenge.t -> Api_types.Sca_info.t
val kyc : Kyc_core.Kyc_case.Customer_view.t -> Api_types.Kyc.Status.t
val balance : Banking_provider.Account.Balance.t -> Api_types.Balance.t

val account
  :  ?balance:Banking_provider.Account.Balance.t
  -> Account.t
  -> Api_types.Account.t

val statement : Banking_provider.Statement.t -> Api_types.Statement.t
val beneficiary : Payments_core.Beneficiary.t -> Api_types.Beneficiary.t
val fee_quote : Banking_provider.Transfer_info.Fee_quote.t -> Api_types.Fee_quote.t

(** Lower-case status name, or [under_review] while an operator holds it. *)
val transfer_status : Transfer.t -> string

val transfer : Transfer.t -> Api_types.Transfer.t
val swap : Swap.t -> Api_types.Swap.t
val card : Card.t -> Api_types.Card.t
val standing_order : Standing_order.t -> Api_types.Standing_order.t

(** For operators only: severity and reason codes, never shown to customers. *)
val review : Review.t -> Api_types.Review.t

val schedule_to_string : Payments_core.Schedule.t -> string

(** [daily], [weekly:mon] ... [weekly:sun], [monthly:<1-31>]. *)
val schedule_of_string : string -> Payments_core.Schedule.t Or_error.t

val rail_to_string : Payments_core.Rail.t -> string

(** [sepa], [sepa_instant], [onchain:<network>] ([bitcoin], [ethereum], ...), [lightning],
    [intraledger]. *)
val rail_of_string : string -> Payments_core.Rail.t Or_error.t
