(** KYC from the customer's side and the provider work that follows sign-up.

    Our case ({!Kyc_core.Kyc_case}) is rebuilt from the KYC events in the store: the
    [Opened] event written at sign-up, then the provider's step and status reports. The
    customer sees only {!Kyc_core.Kyc_case.customer_view} (no risk band, screening or
    alert ever leaves it). When the case reaches [Approved], accounts are opened through
    the outbox. *)

open! Core
open Async
open Neobank_api_types
open Model

val case : Ctx.t -> Customer_id.t -> Kyc_core.Kyc_case.t Problem.res
val status : Ctx.t -> Customer_id.t -> Api_types.Kyc.Status.t Problem.res

(** [email], [mobile] (a code is sent), [identity_document], [liveness] (a hosted-flow
    token for the provider's SDK), [address_proof], [source_of_funds]. *)
val step_of_string : string -> Kyc_core.Verification.Step.t Or_error.t

val start
  :  Ctx.t
  -> Customer.t
  -> Api_types.Kyc.Start_request.t
  -> Api_types.Kyc.Session.t Problem.res

val complete
  :  Ctx.t
  -> Customer.t
  -> Api_types.Kyc.Complete_request.t
  -> Api_types.Kyc.Status.t Problem.res

(** Outbox work: creates the customer at the provider (idempotent on our customer id) and
    records the provider's reference. *)
val create_provider_customer : Ctx.t -> Customer_id.t -> unit Or_error.t Deferred.t

(** Outbox work: opens an account per configured currency the customer lacks at the
    provider, asks for IBANs and addresses, and records them. Safe to repeat. *)
val open_accounts : Ctx.t -> Customer_id.t -> unit Or_error.t Deferred.t

(** The writes a provider KYC event leads to for [customer]: the event on our case when
    the case accepts it, and the accounts job when it approves the case. [None] when the
    case refuses the event (logged and counted; the inbox still records it). *)
val writes_for_event
  :  Ctx.t
  -> Customer.t
  -> Banking_provider.Event.t
  -> (Store.Write.t list * Kyc_core.Kyc_case.Customer_view.t option) Problem.res
