(** Beneficiaries, outgoing transfers and the operator review queue.

    {v
      POST /transfers          draft -> risk-rules -> Block: refused (Cancelled)
                                                   -> Review: held, /ops/review
                                                   -> otherwise: SCA challenge, or the
                                                      exemption risk-rules names
      POST /transfers/:id/confirm-sca
                               password + authenticator code, bound to amount and payee
                               -> outbox: initiate at the provider (transfer id is the
                                  provider's idempotency key)
                               -> the provider may ask for its own code: answer it on
                                  the same route
      webhooks / events        -> Submitted, Pending, Settled, Rejected, Returned, ...
    v}

    The final state always comes from the provider's events, never from the initiating
    response. Every decision is recorded with the risk context it was made on, so it can
    be replayed. *)

open! Core
open Async
open Neobank_api_types
open Model

(** A positive amount in a registered currency. *)
val parse_amount : Api_types.Amount.t -> Money.Amount.t Or_error.t

(** The customer's name from the KYC case: the payer named in SEPA messages and travel
    rule records. *)
val customer_name : Ctx.t -> Customer_id.t -> string Problem.res

(** Rail details for a payment with our id [id]: pacs.008 fields for SEPA (the id is the
    end-to-end id), a travel-rule record for on-chain, nothing for other rails. *)
val details
  :  Ctx.t
  -> id:string
  -> rail:Payments_core.Rail.t
  -> account:Account.t
  -> beneficiary:Payments_core.Beneficiary.t
  -> payer:string
  -> wallet_provider:string option
  -> reference:string
  -> Payments_core.Transfer.Details.t option Or_error.t

(** The beneficiary, if the customer owns it; 404 otherwise. *)
val owned_beneficiary
  :  Ctx.t
  -> Customer_id.t
  -> string
  -> Payments_core.Beneficiary.t Problem.res

(** A default rail from the account's currency, or the requested one if it reaches the
    beneficiary. *)
val pick_rail
  :  account:Account.t
  -> beneficiary:Payments_core.Beneficiary.t
  -> string option
  -> Payments_core.Rail.t Or_error.t

val beneficiaries : Ctx.t -> Customer_id.t -> Api_types.Beneficiary.t list Problem.res

val add_beneficiary
  :  Ctx.t
  -> Customer_id.t
  -> Api_types.Beneficiary.Request.t
  -> Api_types.Beneficiary.t Problem.res

(** Trusted beneficiaries may be exempt from SCA (RTS art. 13), so trusting one needs a
    fresh SCA itself. *)
val trust_beneficiary
  :  Ctx.t
  -> Session.t
  -> string
  -> Api_types.Beneficiary.t Problem.res

val quote_fee
  :  Ctx.t
  -> Customer_id.t
  -> Api_types.Fee_quote.Request.t
  -> Api_types.Fee_quote.t Problem.res

(** Returns the status to answer with ([201] for a new challenge or a submission, [202]
    while held for review) and the transfer. *)
val create_transfer
  :  Ctx.t
  -> Session.t
  -> wallet_provider:string option
  -> Api_types.Transfer.Request.t
  -> (Cohttp.Code.status_code * Api_types.Transfer.t) Problem.res

(** A fresh challenge for a transfer waiting for SCA (the first one expired, or an
    operator released the transfer). *)
val restart_sca : Ctx.t -> Customer_id.t -> string -> Api_types.Transfer.t Problem.res

(** Answers the open challenge: ours (password and authenticator code) or, after that, the
    provider's (its code in [code]). *)
val confirm_sca
  :  Ctx.t
  -> Customer_id.t
  -> string
  -> Api_types.Transfer.Confirm_request.t
  -> Api_types.Transfer.t Problem.res

(** Before submission only. *)
val cancel_transfer : Ctx.t -> Customer_id.t -> string -> Api_types.Transfer.t Problem.res

val get_transfer : Ctx.t -> Customer_id.t -> string -> Api_types.Transfer.t Problem.res
val list_transfers : Ctx.t -> Customer_id.t -> Api_types.Transfer.t list Problem.res

(** Outbox work: sends an authorized transfer to the provider. Idempotent: a transfer
    already past submission is left alone, and the provider dedups on the transfer id. *)
val initiate : Ctx.t -> Payments_core.Transfer.Id.t -> unit Or_error.t Deferred.t

(** Applies a provider's report; [None] when it changes nothing (a redelivery or an
    out-of-order report). *)
val apply_update
  :  Ctx.t
  -> Transfer.t
  -> Banking_provider.Transfer_update.t
  -> at:Time_ns.t
  -> Transfer.t option Or_error.t

(** {2 Operators} *)

val reviews : Ctx.t -> pending_only:bool -> Api_types.Review.t list Problem.res

(** [approve] releases the transfer to SCA (or straight to the provider when exempt);
    [reject] cancels it. *)
val decide_review
  :  Ctx.t
  -> string
  -> Api_types.Review.Decide_request.t
  -> Api_types.Review.t Problem.res
