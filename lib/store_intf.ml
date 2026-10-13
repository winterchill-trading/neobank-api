(** Where the API keeps its application state, in domain types.

    Two implementations: {!Store_memory} (tests, offline mode) and {!Store_pg} (PostgreSQL
    through pg-async). No SQL, table name or pg-async type appears outside [store_pg.ml]
    and [migrations/]; the same conformance suite runs over both.

    {b Writes are batches.} Every change goes through {!S.commit}, which applies a list of
    {!Write.t}s atomically: a transfer's new state together with its outbox message, a
    provider event's effects together with its inbox row. A batch that would break a
    uniqueness rule (an e-mail already registered, a card request id reused) or that
    carries an inbox row for an event already applied changes nothing and says so.

    {b Reads tolerate replicas.} Nothing here assumes a read sees a write made through
    another store handle; callers that need their own write read the value they wrote. *)

open! Core
open Async_kernel
open Model

module Write = struct
  type t =
    | Insert_customer of Customer.t (** [Conflict] when the id or e-mail exists *)
    | Update_customer of Customer.t
    | Put_session of Session.t
    | Delete_session of { token_hash : string }
    | Kyc_event of Kyc_core.Event.t
    (** appended to the customer's case; an event id already there is skipped *)
    | Put_account of Account.t
    | Put_beneficiary of Payments_core.Beneficiary.t
    | Put_transfer of Transfer.t
    | Put_swap of Swap.t
    | Insert_card of Card.t (** [Conflict] when the request id was used *)
    | Put_card of Card.t
    | Put_standing_order of Standing_order.t
    | Put_review of Review.t
    | Enqueue of Outbox.t
    | Update_outbox of Outbox.t
    | Inbox of Inbox_entry.t
    (** the batch is a [Duplicate_event] when this event id was applied before *)
    | Audit of Audit.Record.t (** appended to the hash chain *)
  [@@deriving sexp_of]
end

module Commit_result = struct
  type t =
    | Committed
    | Duplicate_event
    (** the batch's inbox event was applied before; nothing was written *)
    | Conflict of string (** a uniqueness rule; nothing was written *)
  [@@deriving sexp, compare, equal]
end

module Pruned = struct
  type t =
    { idempotency_keys : int
    ; inbox_rows : int
    ; sessions : int
    }
  [@@deriving sexp, compare, equal]
end

module type S = sig
  type t

  (** ["memory"] or ["postgres"], for logs and [/healthz]. *)
  val kind : string

  val commit : t -> Write.t list -> Commit_result.t Or_error.t Deferred.t

  (** {2 Customers and sessions} *)

  val customer : t -> Customer_id.t -> Customer.t option Or_error.t Deferred.t

  (** By {!Customer.normalize_email}d address. *)
  val customer_by_email : t -> string -> Customer.t option Or_error.t Deferred.t

  val customer_by_provider_ref
    :  t
    -> Customer_ref.t
    -> Customer.t option Or_error.t Deferred.t

  val session : t -> token_hash:string -> Session.t option Or_error.t Deferred.t

  (** In the order appended. *)
  val kyc_events : t -> Customer_id.t -> Kyc_core.Event.t list Or_error.t Deferred.t

  (** {2 Products} *)

  val account : t -> Account_ref.t -> Account.t option Or_error.t Deferred.t

  (** Ordered by currency code (byte order), then id. *)
  val accounts : t -> Customer_id.t -> Account.t list Or_error.t Deferred.t

  val beneficiary
    :  t
    -> Payments_core.Beneficiary.Id.t
    -> Payments_core.Beneficiary.t option Or_error.t Deferred.t

  (** Oldest first. *)
  val beneficiaries
    :  t
    -> Customer_id.t
    -> Payments_core.Beneficiary.t list Or_error.t Deferred.t

  val transfer
    :  t
    -> Payments_core.Transfer.Id.t
    -> Transfer.t option Or_error.t Deferred.t

  (** Newest first; [since] filters on [created_at]. *)
  val transfers
    :  t
    -> Customer_id.t
    -> ?since:Time_ns.t
    -> unit
    -> Transfer.t list Or_error.t Deferred.t

  val swap : t -> Payments_core.Swap.Id.t -> Swap.t option Or_error.t Deferred.t

  (** Newest first. *)
  val swaps : t -> Customer_id.t -> Swap.t list Or_error.t Deferred.t

  val card : t -> Card_core.Ids.Card_id.t -> Card.t option Or_error.t Deferred.t
  val card_by_request : t -> request_id:string -> Card.t option Or_error.t Deferred.t

  (** Oldest first. *)
  val cards : t -> Customer_id.t -> Card.t list Or_error.t Deferred.t

  val standing_order
    :  t
    -> Payments_core.Standing_order.Id.t
    -> Standing_order.t option Or_error.t Deferred.t

  val standing_order_by_provider_ref
    :  t
    -> Banking_provider.Standing_order_ref.t
    -> Standing_order.t option Or_error.t Deferred.t

  (** Oldest first. *)
  val standing_orders : t -> Customer_id.t -> Standing_order.t list Or_error.t Deferred.t

  (** {2 Operators} *)

  val review : t -> string -> Review.t option Or_error.t Deferred.t

  (** Oldest first; only pending ones with [pending_only]. *)
  val reviews : t -> pending_only:bool -> Review.t list Or_error.t Deferred.t

  (** {2 Outbox, inbox, idempotency, audit} *)

  (** Pending messages with [next_attempt_at <= now], oldest first. *)
  val outbox_due : t -> now:Time_ns.t -> limit:int -> Outbox.t list Or_error.t Deferred.t

  val outbox : t -> string -> Outbox.t option Or_error.t Deferred.t

  (** Every message in a state, oldest first (for operators and tests). *)
  val outbox_in_state : t -> state:string -> Outbox.t list Or_error.t Deferred.t

  val inbox_mem : t -> Banking_provider.Event_id.t -> bool Or_error.t Deferred.t

  (** Starts a request under [scope]: [Started] for a new key (now [In_progress]),
      [Replay] for a completed one with the same [request_hash], [Mismatch] for a
      different hash, [In_progress] while the first request runs. *)
  val idempotency_begin
    :  t
    -> Idempotency.Scope.t
    -> request_hash:string
    -> now:Time_ns.t
    -> Idempotency.Begin.t Or_error.t Deferred.t

  (** Stores the response; later requests with the key replay it. *)
  val idempotency_complete
    :  t
    -> Idempotency.Scope.t
    -> status:int
    -> body:string
    -> unit Or_error.t Deferred.t

  (** Forgets a key whose request was not executed (it failed validation), so a corrected
      retry may use it. *)
  val idempotency_abandon : t -> Idempotency.Scope.t -> unit Or_error.t Deferred.t

  (** The whole chain, oldest first. *)
  val audit_log : t -> Audit.t list Or_error.t Deferred.t

  (** Deletes idempotency keys created before [idempotency_before], inbox rows received
      before [inbox_before] and sessions idle since before [sessions_before]. *)
  val prune
    :  t
    -> idempotency_before:Time_ns.t
    -> inbox_before:Time_ns.t
    -> sessions_before:Time_ns.t
    -> Pruned.t Or_error.t Deferred.t
end

module type Store = sig
  module Write = Write
  module Commit_result = Commit_result
  module Pruned = Pruned

  module type S = S
end
