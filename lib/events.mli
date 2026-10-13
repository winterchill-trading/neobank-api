(** Applies the provider's events to our records, each exactly once.

    Events come from two places: the provider's event stream ({!Bank.events}, the fake's
    in-process feed or a poller) and [POST /webhooks/provider], whose signature is checked
    on the raw bytes by the provider adapter before anything parses them. Both end here.
    Each event's effects are committed in one batch with its inbox row (keyed by the
    provider's event id), so a redelivery, or the same event arriving by both routes,
    changes nothing the second time. Events are applied one at a time, and in whatever
    order they come: the state machines ignore reports that were overtaken. *)

open! Core
open Async

val apply
  :  Ctx.t
  -> Banking_provider.Event.t
  -> [ `Applied | `Duplicate ] Or_error.t Deferred.t

(** Verifies and applies one delivery; the counts of new and repeated events. *)
val ingest_webhook
  :  Ctx.t
  -> Banking_provider.Webhook.Request.t
  -> (int * int) Problem.res

(** Applies everything {!Bank.events} yields until the returned function is called. *)
val start : Ctx.t -> unit -> unit
