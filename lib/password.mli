(** Password hashing with PBKDF2-HMAC-SHA256 (RFC 8018).

    Argon2 would be the first choice for new password storage, but no Argon2 binding
    builds on the OxCaml switch and cryptokit 1.16 has no KDF, so PBKDF2 is written here
    over cryptokit's HMAC-SHA256, with the iteration count OWASP gives for it (600 000)
    and a 16-byte random salt. The stored form names the algorithm and parameters, so a
    later move to another scheme can rehash on the next login.

    Stored form: [pbkdf2-sha256$<iterations>$<salt, hex>$<derived key, hex>]. *)

open! Core

(** [PBKDF2(HMAC-SHA256, password, salt, iterations, length)]. *)
val pbkdf2_sha256
  :  password:string
  -> salt:string
  -> iterations:int
  -> length:int
  -> string

val default_iterations : int

(** The stored form for [password] with a fresh random salt. [iterations] is for tests. *)
val hash : ?iterations:int -> string -> string

(** Recomputes and compares in constant time. A malformed stored form is [false]. *)
val verify : stored:string -> string -> bool
