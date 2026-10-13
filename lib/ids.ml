open! Core

type t =
  | Random
  | Counter of int ref

let random () = Random
let counter () = Counter (ref 0)
let hex s = Cryptokit.transform_string (Cryptokit.Hexa.encode ()) s

let next t prefix =
  match t with
  | Random ->
    sprintf "%s-%s" prefix (hex (Cryptokit.Random.string Cryptokit.Random.secure_rng 8))
  | Counter n ->
    incr n;
    sprintf "%s-%06d" prefix !n
;;

let token t =
  match t with
  | Random -> hex (Cryptokit.Random.string Cryptokit.Random.secure_rng 32)
  | Counter n ->
    incr n;
    sprintf "token-%06d" !n
;;
