(** ICT incidents under DORA (Regulation 2022/2554): the facts an operator records, the
    classification they lead to (RTS 2024/1772 Art 8-9) and the report deadlines (RTS
    2025/301 Art 5).

    An incident is {e major} when a critical service is affected and either data was lost
    or at least two of the other criteria are met. A major incident's initial notification
    is due 4 hours after classification and no later than 24 hours after detection; the
    intermediate report 72 hours after the initial notification; the final report one
    month after the latest intermediate report. Pure: no clock, no store. *)

open! Core

module Criterion : sig
  type t =
    | Clients (** > 10 % of the service's clients or > 100 000 clients *)
    | Reputation
    | Duration (** > 24 h, or > 2 h of downtime of a critical service *)
    | Geography (** two or more Member States *)
    | Data_loss (** availability, authenticity, integrity, confidentiality, or a breach *)
    | Economic (** costs and losses > EUR 100 000 *)
  [@@deriving sexp, compare, equal, enumerate]
end

module Severity : sig
  type t =
    | Minor
    | Major
  [@@deriving sexp, compare, equal]
end

module Status : sig
  type t =
    | Open
    | Mitigated
    | Resolved
  [@@deriving sexp, compare, equal]
end

module Report : sig
  type kind =
    | Initial
    | Intermediate
    | Final
  [@@deriving sexp, compare, equal]

  type t =
    { kind : kind
    ; sent_at : Time_ns.Alternate_sexp.t
    }
  [@@deriving sexp, compare, equal]
end

type t =
  { id : string
  ; detected_at : Time_ns.Alternate_sexp.t
  ; classified_at : Time_ns.Alternate_sexp.t option
  ; critical_service_affected : bool
  ; services_affected : string list
  ; clients_affected : int
  ; clients_of_service : int
  ; duration : Time_ns.Span.t
  ; downtime : Time_ns.Span.t
  ; member_states : string list
  ; data_impact : bool
  ; reputational_impact : bool
  ; costs_eur : Money.Amount.t
  ; root_cause : string
  ; status : Status.t
  ; reports : Report.t list (** oldest first *)
  }
[@@deriving sexp, compare, equal]

val criteria_met : t -> Criterion.t list
val severity : t -> Severity.t

module Deadlines : sig
  type t =
    { initial : Time_ns.Alternate_sexp.t
    ; intermediate : Time_ns.Alternate_sexp.t
    ; final : Time_ns.Alternate_sexp.t option
    (** one month after the latest intermediate report, once one was sent *)
    }
  [@@deriving sexp, compare, equal]
end

(** [None] for a minor or not yet classified incident. Later deadlines count from the
    reports actually sent when there are some, from the earlier deadline otherwise. *)
val deadlines : t -> Deadlines.t option

(** Reports due and not sent at [now], with their deadline. *)
val overdue : t -> now:Time_ns.t -> (Report.kind * Time_ns.t) list
