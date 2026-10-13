open! Core

type t =
  { status : Cohttp.Code.status_code
  ; code : string
  ; message : string
  ; sca : Neobank_api_types.Api_types.Sca_info.t option
  }

let sexp_of_t t =
  [%sexp
    { status = (Cohttp.Code.code_of_status t.status : int)
    ; code : string = t.code
    ; message : string = t.message
    }]
;;

let create ?sca status code message = { status; code; message; sca }
let bad_request code message = create `Bad_request code message
let unauthorized code message = create `Unauthorized code message
let forbidden code message = create `Forbidden code message
let not_found what = create `Not_found "not_found" (sprintf "no such %s" what)
let conflict code message = create `Conflict code message
let unprocessable code message = create `Unprocessable_entity code message

let bad_gateway e =
  create `Bad_gateway "provider_error" ("the provider refused: " ^ Error.to_string_hum e)
;;

let internal (_ : Error.t) = create `Internal_server_error "internal" "internal error"

let of_validation = function
  | Ok x -> Ok x
  | Error e -> Error (bad_request "invalid_request" (Error.to_string_hum e))
;;

let of_internal = function
  | Ok x -> Ok x
  | Error e -> Error (internal e)
;;

let of_provider = function
  | Ok x -> Ok x
  | Error e -> Error (bad_gateway e)
;;

type 'a res = ('a, t) Result.t Async.Deferred.t
