(** The one set of masking rules for logs, audit records and operator screens.

    - Card numbers: the last four digits only ([**** 1234]), stricter than the BIN plus
      last four PCI DSS 3.4.1 allows.
    - IBANs: country code and last four ([DE** **** 1234]).
    - E-mail addresses: first character and domain ([a***@example.com]).
    - Crypto addresses: first six and last four characters.
    - OTPs, CVVs, PINs, passwords, API keys, session tokens and webhook secrets are never
      shown at all: {!secret} prints a fixed placeholder whatever it is given. *)

open! Core

val pan : string -> string
val iban : string -> string
val email : string -> string
val crypto_address : string -> string

(** Always ["<redacted>"]. *)
val secret : string -> string

(** Masks a destination as typed by a customer: an IBAN, an e-mail-like Lightning address,
    or anything else as a crypto address. *)
val destination : string -> string
