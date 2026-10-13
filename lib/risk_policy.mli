(** The risk policy the API evaluates every outgoing transfer against
    ({!Risk_rules.Policy}), and how a customer maps to a policy tier.

    The default policy follows risk-rules' example: sanctions geography, velocity,
    structuring, money mules, account takeover, push payment scams, crypto travel-rule
    checks, per-tier caps and SCA exemptions; per tier, a payment above a first cap is
    held for an operator ([/ops/review]) and one above a second cap is refused.
    [-policy FILE] replaces it. *)

open! Core

val default_text : string
val default : Risk_rules.Policy.t

(** [verified] from kyc-core tier T2 up, [basic] below. *)
val tier_of_kyc : Kyc_core.Tier.t -> Risk_rules.Tier.t
