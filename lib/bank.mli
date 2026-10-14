(** The banking provider the API runs on, whichever implementation it is.

    A value packs an implementation of {!Banking_provider.Provider.S} with, when it has
    one, its {!Banking_provider.Sandbox.S} (simulated deposits, KYC decisions, card
    authorizations). The service calls these functions and never names an implementation;
    [-provider fake] and [-provider striga] differ only in how the value is built. *)

open! Core
open Async
open Banking_provider
open Payments_core

type t

(** How a provider's hosted card-number component is driven from the browser: the options
    it is created with, and the consent challenge it starts. Not part of {!Provider.S}
    because it is browser-side and provider-specific. *)
module Card_ui : sig
  type display =
    { provider : string
    ; script_url : string option
    ; application_id : string option
    ; ui_secret : Sensitive.t option (** for the browser component only *)
    ; user_id : string option
    ; card_id : string
    }
  [@@deriving sexp_of]

  type t =
    { display : Card_core.Ids.Card_id.t -> display Or_error.t Deferred.t
    ; consent_started :
        Card_core.Ids.Card_id.t
        -> challenge_id:string
        -> Sca_challenge.t Or_error.t Deferred.t
    }
end

val of_provider
  :  (module Provider.S with type t = 'a)
  -> ?sandbox:(module Sandbox.S with type t = 'a)
  -> ?card_ui:Card_ui.t
  -> 'a
  -> t

(** The deterministic fake over a ledger journal at [state_dir/journal] (created, or
    reopened when it exists), with [time_source] for its clock. The journal is the books
    of the simulated bank: every simulated movement is booked there. *)
val fake
  :  ?config:Fake_provider.Config.t
  -> time_source:[> read ] Time_source.T1.t
  -> state_dir:string
  -> unit
  -> t Or_error.t

(** The ledger journal the bank books into, when the API keeps it (the fake's books);
    {!Ledger_projection} copies it into the store for queries. *)
val journal : t -> Ledger.Journal.t option

(** The fake's own handle, when the bank is the fake (tests, demo tools). *)
val as_fake : t -> Fake_provider.t option

val name : t -> string
val has_sandbox : t -> bool

(** Releases what {!fake} opened (the journal). *)
val close : t -> unit

val create_customer
  :  t
  -> customer_id:Kyc_core.Customer_id.t
  -> person:Kyc_core.Person.t
  -> Customer.t Or_error.t Deferred.t

val start_verification
  :  t
  -> Customer_ref.t
  -> Kyc_core.Verification.Step.t
  -> Customer.Verification_session.t Or_error.t Deferred.t

val complete_verification
  :  t
  -> Customer_ref.t
  -> Kyc_core.Verification.Step.t
  -> code:Sensitive.t
  -> Kyc_core.Verification.State.t Or_error.t Deferred.t

val kyc_status : t -> Customer_ref.t -> Customer.Kyc_status.t Or_error.t Deferred.t

val open_account
  :  t
  -> Customer_ref.t
  -> Money.Currency.t
  -> Account.t Or_error.t Deferred.t

val accounts : t -> Customer_ref.t -> Account.t list Or_error.t Deferred.t
val balance : t -> Account_ref.t -> Account.Balance.t Or_error.t Deferred.t
val enrich_account : t -> Account_ref.t -> Account.t Or_error.t Deferred.t

val statement
  :  t
  -> Account_ref.t
  -> from:Time_ns.t
  -> until:Time_ns.t
  -> Statement.t Or_error.t Deferred.t

val quote_fee
  :  t
  -> from:Account_ref.t
  -> rail:Rail.t
  -> Money.Amount.t
  -> Transfer_info.Fee_quote.t Or_error.t Deferred.t

val initiate_transfer
  :  t
  -> from:Account_ref.t
  -> Transfer.t
  -> Transfer_info.t Sca_challenge.Or_challenge.t Or_error.t Deferred.t

val transfer : t -> Transfer.Id.t -> Transfer_info.t Or_error.t Deferred.t

val confirm_sca
  :  t
  -> Sca.Id.t
  -> code:Sensitive.t
  -> Sca_challenge.Result.t Or_error.t Deferred.t

val quote_swap
  :  t
  -> from:Account_ref.t
  -> to_:Account_ref.t
  -> sell:Money.Amount.t
  -> Swap.Quote.t Or_error.t Deferred.t

val execute_swap
  :  t
  -> Swap.Id.t
  -> unit Sca_challenge.Or_challenge.t Or_error.t Deferred.t

val issue_virtual_card
  :  t
  -> request_id:string
  -> Customer_ref.t
  -> account:Account_ref.t
  -> Card_info.t Or_error.t Deferred.t

val card : t -> Card_core.Ids.Card_id.t -> Card_info.t Or_error.t Deferred.t

val set_card_status
  :  t
  -> Card_core.Ids.Card_id.t
  -> Card_core.Card.Status.t
  -> Card_info.t Or_error.t Deferred.t

val set_card_limits
  :  t
  -> Card_core.Ids.Card_id.t
  -> Card_core.Limits.t
  -> Card_info.t Or_error.t Deferred.t

val card_details_token
  :  t
  -> Card_core.Ids.Card_id.t
  -> Card_core.Pan_display_token.t Sca_challenge.Or_challenge.t Or_error.t Deferred.t

val create_standing_order
  :  t
  -> from:Account_ref.t
  -> Standing_order.t
  -> Standing_order_ref.t Sca_challenge.Or_challenge.t Or_error.t Deferred.t

(** The provider's component options; without a {!Card_ui}, just the provider's name and
    the card id (the fake shows nothing in a browser). *)
val card_display : t -> Card_core.Ids.Card_id.t -> Card_ui.display Or_error.t Deferred.t

(** The challenge behind a consent the browser component started; without a {!Card_ui},
    the challenge {!card_details_token} asks for, if any. *)
val card_consent_started
  :  t
  -> Card_core.Ids.Card_id.t
  -> challenge_id:string
  -> Sca_challenge.t Or_error.t Deferred.t

val cancel_standing_order : t -> Standing_order_ref.t -> unit Or_error.t Deferred.t
val events : t -> Event.t Pipe.Reader.t
val ingest_webhook : t -> Webhook.Request.t -> Event.t list Or_error.t Deferred.t

(** {2 Sandbox}
    Each fails when the provider has no sandbox. *)

val simulate_deposit
  :  t
  -> account:Account_ref.t
  -> rail:Rail.t
  -> Money.Amount.t
  -> Transfer.Id.t Or_error.t Deferred.t

val simulate_kyc_decision
  :  t
  -> Customer_ref.t
  -> Sandbox.Kyc_decision.t
  -> unit Or_error.t Deferred.t

val simulate_card_auth
  :  t
  -> Card_core.Ids.Card_id.t
  -> Card_core.Merchant.t
  -> Card_core.Response_code.t Or_error.t Deferred.t

val simulate_transfer_outcome
  :  t
  -> Transfer.Id.t
  -> Transfer_update.t
  -> unit Or_error.t Deferred.t
