(** {!Store.S} over PostgreSQL through pg-async.

    The only module that knows SQL, table names or pg-async types. Each record is one row:
    the columns queries filter and sort on, and the record's Bin_prot bytes in [body]. A
    {!Store.Write.t} batch runs in one serializable transaction, retried by pg-async on
    serialization failures; uniqueness rules are the table constraints.

    {!connect} applies the embedded migrations ([migrations/*.sql]) before returning,
    under pg-async's advisory lock, so several replicas can start at once. *)

open! Core
open Async

type t

(** [url] as libpq takes it, e.g. [postgres://neobank@127.0.0.1:5433/neobank]; without
    one, [DATABASE_URL] or the [PG*] variables. TLS is required unless the URL says
    [sslmode=disable] (pg-async's default). *)
val connect : ?max_size:int -> ?url:string -> unit -> t Or_error.t Deferred.t

val close : t -> unit Deferred.t

(** The connection target with the password masked, for logs. *)
val describe : t -> string

(** Versions applied by the last {!connect}. *)
val applied_migrations : t -> int list

include Store.S with type t := t
