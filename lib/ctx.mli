(** What every part of the service shares: settings, the store, the provider, the clock,
    id generation, the risk policy, metrics, the push hub and per-record locks. *)

open! Core
open Async

module Config : sig
  type t =
    { password_iterations : int (** PBKDF2 rounds; lowered only in tests *)
    ; session_idle : Time_ns.Span.t (** 5 minutes (RTS art. 4(3)(d)) *)
    ; session_max : Time_ns.Span.t (** absolute session lifetime *)
    ; sca_fresh_for : Time_ns.Span.t
    (** how long a login or step-up counts as fresh SCA for sensitive operations
        (revealing a card number, changing limits, swaps, trusting a payee) *)
    ; challenge_ttl : Time_ns.Span.t (** to answer a payment's SCA challenge *)
    ; totp_issuer : string (** shown by authenticator apps *)
    ; currencies : Money.Currency.t list (** accounts opened after KYC approval *)
    ; indicative_eur : (Money.Currency.t * string) list
    (** EUR per unit, for regulatory thresholds only (travel rule, confirmations); prices
        for customers always come from the provider's quotes *)
    ; relay_interval : Time_ns.Span.t (** outbox poll, 1 s *)
    ; relay_backoff_max : Time_ns.Span.t (** backoff doubles from 1 s to this, 5 min *)
    ; relay_give_up_after : Time_ns.Span.t (** then [Needs_review], 24 h *)
    ; idempotency_ttl : Time_ns.Span.t (** 24 h *)
    ; inbox_ttl : Time_ns.Span.t (** 30 days *)
    ; ops_token : string option (** bearer token for [/ops/*]; [None] disables them *)
    ; kyc_policy : Kyc_core.Kyc_case.Policy.t
    }

  (** The values above, PBKDF2 at 600 000 rounds, EUR, BTC, Lightning BTC, ETH, USDC and
      USDT accounts, BTC 60 000 / ETH 3 000 / stablecoins 1 EUR, and {!demo_kyc_policy}. *)
  val default : t

  (** kyc-core's default policy without the screening and risk-rating requirements: the
      demo has no screening list or rating pipeline yet, so it trusts the provider's KYC
      decision for those (a known gap, listed in the README). *)
  val demo_kyc_policy : Kyc_core.Kyc_case.Policy.t
end

type t =
  { config : Config.t
  ; store : Any_store.t
  ; bank : Bank.t
  ; time_source : Time_source.t
  ; ids : Ids.t
  ; policy : Risk_rules.Policy.t
  ; metrics : Metrics.t
  ; hub : Hub.t
  ; locks : Keyed_lock.t
  ; mutable kick : unit -> unit Deferred.t
  (** runs the outbox relay once now; set by {!Relay.start} *)
  ; provider_challenges :
      (Kyc_core.Customer_id.t * Banking_provider.Sca_challenge.Purpose.t) String.Table.t
  (** open provider challenges answered through [POST /sca/:id], by challenge id: who may
      answer each and what it is for. In memory: after a restart the customer asks again. *)
  }

val create
  :  ?ids:Ids.t
  -> ?time_source:Time_source.t
  -> ?policy:Risk_rules.Policy.t
  -> config:Config.t
  -> store:Any_store.t
  -> bank:Bank.t
  -> unit
  -> t

val now : t -> Time_ns.t

(** A store read; a failure is an internal error. *)
val read : 'a Or_error.t Deferred.t -> 'a Problem.res

(** A store read that must find something: [None] is 404 naming [what]. *)
val find : what:string -> 'a option Or_error.t Deferred.t -> 'a Problem.res

(** Commits a batch; a [Conflict] is 409 with its name as the code, a [Duplicate_event] is
    fine. *)
val commit : t -> Store.Write.t list -> unit Problem.res

val audit
  :  t
  -> actor:string
  -> ?on_behalf_of:string
  -> action:string
  -> object_id:string
  -> outcome:string
  -> unit
  -> Store.Write.t

(** EUR value of an amount at the indicative rate, when one is configured. *)
val eur_value : t -> Money.Amount.t -> Money.Amount.t option
