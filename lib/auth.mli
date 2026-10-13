(** Sign-up, sessions and strong customer authentication for logins.

    A login is SCA: the password (knowledge) and an authenticator code (possession). The
    first successful login also confirms the authenticator enrolled at sign-up. Wrong
    answers never say which factor was wrong; five in a row lock the customer for 30
    minutes, and a second lock within a day is permanent until re-enabled
    ({!Payments_core.Sca.Lock}).

    Sessions are bearer tokens of 32 random bytes; the store keeps only their SHA-256. A
    session ends after 5 minutes without a request (RTS art. 4(3)(d)) or 12 hours in all.
    Balances and history need only a live session; payments are confirmed with their own
    challenge, and other sensitive operations need a login or step-up within the last 5
    minutes ({!require_fresh_sca}). *)

open! Core
open Neobank_api_types
open Model

(** Validates the person, stores the customer with a new authenticator secret, opens the
    KYC case and queues the provider customer. The response carries the secret once, for
    the authenticator app. *)
val signup
  :  Ctx.t
  -> Api_types.Auth.Signup_request.t
  -> Api_types.Auth.Signup_response.t Problem.res

val login
  :  Ctx.t
  -> Api_types.Auth.Login_request.t
  -> Api_types.Auth.Session.t Problem.res

(** The session behind a bearer token, touched; 401 when missing, idle too long or too
    old. *)
val authenticate : Ctx.t -> token:string -> Session.t Problem.res

(** Password and authenticator again; refreshes the session's SCA time. *)
val step_up
  :  Ctx.t
  -> Session.t
  -> Api_types.Auth.Step_up_request.t
  -> Api_types.Auth.Session.t Problem.res

val logout : Ctx.t -> Session.t -> unit Problem.res

(** 403 [sca_required] unless the session's last SCA is recent. *)
val require_fresh_sca : Ctx.t -> Session.t -> unit Problem.res

val sca_is_fresh : Ctx.t -> Session.t -> bool

(** Checks a password and an authenticator code together, in a thread (PBKDF2 is
    deliberately slow). On success the customer comes back with the code's step recorded,
    so the code cannot be replayed. *)
val check_factors
  :  Ctx.t
  -> Customer.t
  -> password:string
  -> totp:string
  -> [ `Ok of Customer.t | `Wrong ] Async.Deferred.t

(** Records a failed SCA attempt on the customer (lock bookkeeping and the 24-hour count
    risk rules read); [`Locked] when this attempt locked the customer. *)
val record_failure : Ctx.t -> Customer.t -> Customer.t * [ `Locked | `Not_locked ]

val record_success : Customer.t -> Customer.t

(** 423 [locked] when the customer is locked now. *)
val check_not_locked : Ctx.t -> Customer.t -> unit Problem.res

(** Failed SCA attempts in the last 24 hours. *)
val failed_sca_24h : Ctx.t -> Customer.t -> int
