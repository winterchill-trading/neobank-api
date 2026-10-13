(** Authenticator-app enrolment and checks (RFC 6238, HMAC-SHA1, 6 digits, 30 s steps),
    over {!Payments_core.Sca.Totp}.

    A code is accepted for the current step and one step either side (phone clocks drift),
    and each step is accepted once per customer: {!check} returns the step it matched,
    which the caller stores, and refuses steps at or before [last_step], so a code read
    over someone's shoulder cannot be replayed within its window. *)

open! Core

(** RFC 4648 base32 without padding, the form authenticator apps take. *)
val base32_encode : string -> string

val base32_decode : string -> string Or_error.t

(** 20 random bytes (the RFC 4226 recommended length). *)
val generate_secret : unit -> string

(** [otpauth://totp/<issuer>:<account>?secret=...&issuer=...], for a QR code. *)
val otpauth_uri : issuer:string -> account:string -> secret:string -> string

(** The code for [time]; for tests and the demo tools. *)
val code : secret:string -> time:Time_ns.t -> string

(** The matched 30-second step (Unix seconds / 30), or [None] for a wrong code or a step
    not after [last_step]. *)
val check
  :  secret:string
  -> time:Time_ns.t
  -> last_step:int option
  -> string
  -> int option
