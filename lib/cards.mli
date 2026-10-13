(** Virtual cards: issue, freeze and unfreeze, limits, and the number shown through the
    provider's hosted component. The full number, CVV and PIN never reach this service:
    [pan-token] returns a short-lived token for the provider's UI and nothing else.
    Revealing the number and changing limits need a fresh SCA; the provider may also ask
    for its own code ([POST /sca/:id]). *)

open! Core
open Neobank_api_types
open Model

(** [request_id] is derived from the request's idempotency key, so a retried request
    returns the card the first one issued. *)
val issue
  :  Ctx.t
  -> Session.t
  -> request_id:string
  -> Api_types.Card.Issue_request.t
  -> Api_types.Card.t Problem.res

val list : Ctx.t -> Customer_id.t -> Api_types.Card.t list Problem.res
val get : Ctx.t -> Customer_id.t -> string -> Api_types.Card.t Problem.res
val freeze : Ctx.t -> Customer_id.t -> string -> Api_types.Card.t Problem.res
val unfreeze : Ctx.t -> Customer_id.t -> string -> Api_types.Card.t Problem.res

val set_limits
  :  Ctx.t
  -> Session.t
  -> string
  -> Api_types.Card.Limits.t
  -> Api_types.Card.t Problem.res

(** [`Token] once allowed; [`Challenge] when the provider wants its code first: answer it
    with [POST /sca/:id], then ask again. *)
val pan_token
  :  Ctx.t
  -> Session.t
  -> string
  -> [ `Token of Api_types.Card.Pan_token.t | `Challenge of Api_types.Sca_info.t ]
       Problem.res

(** A card's status as the provider reported it. *)
val status_changed
  :  Ctx.t
  -> Card_core.Ids.Card_id.t
  -> Card_core.Card.Status.t
  -> Store.Write.t list Problem.res
