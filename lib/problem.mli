(** A refusal the API answers with: an HTTP status, a stable machine-readable code and a
    message for people. Messages never name which authentication factor failed, never
    reveal risk reasons (a held payment reads "under review"), and never echo secrets. *)

open! Core

type t =
  { status : Cohttp.Code.status_code
  ; code : string
  ; message : string
  ; sca : Neobank_api_types.Api_types.Sca_info.t option
  (** set for [sca_required] answers: the challenge to answer *)
  }
[@@deriving sexp_of]

val create
  :  ?sca:Neobank_api_types.Api_types.Sca_info.t
  -> Cohttp.Code.status_code
  -> string
  -> string
  -> t

val bad_request : string -> string -> t
val unauthorized : string -> string -> t
val forbidden : string -> string -> t
val not_found : string -> t
val conflict : string -> string -> t
val unprocessable : string -> string -> t
val bad_gateway : Error.t -> t
val internal : Error.t -> t

(** [Error e] as [bad_request "invalid_request" (message of e)]. *)
val of_validation : 'a Or_error.t -> ('a, t) Result.t

(** A store or other internal failure: 500, logged by the server, message generic. *)
val of_internal : 'a Or_error.t -> ('a, t) Result.t

(** A provider failure: 502 with the provider's message. *)
val of_provider : 'a Or_error.t -> ('a, t) Result.t

type 'a res = ('a, t) Result.t Async.Deferred.t
