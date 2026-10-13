(** The HTTP and websocket server.

    {v
GET    /healthz                               liveness: store and provider kind
GET    /metrics                               Prometheus text
POST   /auth/signup                           Auth.Signup_request -> 201 Auth.Signup_response
POST   /auth/login                            Auth.Login_request -> Auth.Session
POST   /auth/step-up                   auth   Auth.Step_up_request -> Auth.Session
POST   /auth/logout                    auth   -> 204
GET    /me                             auth   -> Me
GET    /kyc                            auth   -> Kyc.Status
POST   /kyc/start                      auth   Kyc.Start_request -> Kyc.Session
POST   /kyc/complete                   auth   Kyc.Complete_request -> Kyc.Status
GET    /accounts                       auth   -> Account list (with balances)
GET    /accounts/:id                   auth   -> Account
GET    /accounts/:id/statement         auth   ?from=&until= (dates or times), ?format=csv
GET    /beneficiaries                  auth   -> Beneficiary list
POST   /beneficiaries                  auth   Beneficiary.Request -> 201 Beneficiary
POST   /beneficiaries/:id/trust        auth*  -> Beneficiary
POST   /transfers/quote                auth   Fee_quote.Request -> Fee_quote
POST   /transfers                      auth   Transfer.Request -> 201 Transfer | 202 held
GET    /transfers                      auth   -> Transfer list (newest first)
GET    /transfers/:id                  auth   -> Transfer
POST   /transfers/:id/confirm-sca      auth   Transfer.Confirm_request -> Transfer
POST   /transfers/:id/sca              auth   a fresh challenge -> Transfer
POST   /transfers/:id/cancel           auth   -> Transfer
POST   /swaps/quote                    auth   Swap.Quote_request -> 201 Swap
POST   /swaps/:id/execute              auth*  -> Swap
GET    /swaps, /swaps/:id              auth
GET    /cards, /cards/:id              auth
POST   /cards                          auth   Card.Issue_request -> 201 Card
POST   /cards/:id/freeze|unfreeze      auth   -> Card
POST   /cards/:id/limits               auth*  Card.Limits -> Card
POST   /cards/:id/pan-token            auth*  -> Card.Pan_token | 202 Sca_required
GET    /standing-orders                auth
POST   /standing-orders                auth*  Standing_order.Request -> 201 Standing_order
DELETE /standing-orders/:id            auth   -> Standing_order (also POST .../cancel)
POST   /sca/:id                        auth   Sca_confirm_request: a provider's challenge
GET    /ws?token=<token>               auth   websocket of Ws_push
POST   /webhooks/provider                     raw delivery, verified by the provider adapter
GET    /ops/review                     ops    ?all=true -> Review list
POST   /ops/review/:id                 ops    Review.Decide_request -> Review
GET    /ops/outbox                     ops    ?state=needs_review|pending|sent
GET    /ops/audit                      ops    chain check and the latest records
POST   /sandbox/kyc-decision|deposit|card-auth|transfer-outcome   auth (sandbox only)
    v}

    [auth] is [Authorization: Bearer <session token>]; [auth*] also needs a login or
    step-up within the last 5 minutes (403 [sca_required] otherwise); [ops] is
    [Authorization: Bearer <ops token>].

    {b Idempotency.} Every [POST] except [/auth/*] (whose answers carry credentials) and
    [/webhooks/*] (deduplicated by provider event id) needs an [Idempotency-Key] header (1
    to 255 printable ASCII characters). The key is scoped to (customer, method, route,
    key) and bound to the SHA-256 of the canonical JSON body: a retry with the same body
    gets the stored status and body back (with [idempotent-replayed: true]), another body
    gets 422 [idempotency_key_reused], and a retry while the first request runs gets 409
    [idempotency_key_in_use]. Answers to malformed requests (400) and authentication
    failures are not stored. Keys are kept 24 hours.

    Errors are [{"error": code, "message": text}]. Every response carries CORS headers for
    the configured origins. Requests are logged with method, route, status and duration,
    never bodies. *)

open! Core
open! Async

module Config : sig
  type t =
    { allowed_origins : string list (** CORS; ["*"] allows any origin *)
    ; max_body : int (** bytes, 64 KiB *)
    }

  val default : t
end

type t

val create : ?config:Config.t -> Ctx.t -> t
val ctx : t -> Ctx.t

(** [port] 0 picks a free port. *)
val start : t -> port:int -> (Socket.Address.Inet.t, int) Cohttp_async.Server.t Deferred.t

val close : t -> unit

(** The hex SHA-256 of a JSON body with object keys sorted and no spaces ([{}] for an
    empty body): what an idempotency key is bound to. *)
val request_hash : string -> string
