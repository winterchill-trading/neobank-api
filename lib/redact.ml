open! Core

let last n s = String.suffix s n

let pan s =
  let digits = String.filter s ~f:Char.is_digit in
  if String.length digits < 4 then "****" else "**** " ^ last 4 digits
;;

let iban s =
  let s = String.uppercase (String.filter s ~f:Char.is_alphanum) in
  if String.length s < 8
  then "****"
  else sprintf "%s** **** %s" (String.prefix s 2) (last 4 s)
;;

let email s =
  match String.lsplit2 s ~on:'@' with
  | Some (local, domain) when not (String.is_empty local) ->
    sprintf "%c***@%s" local.[0] domain
  | _ -> "***"
;;

let crypto_address s =
  if String.length s <= 12
  then "***"
  else sprintf "%s...%s" (String.prefix s 6) (last 4 s)
;;

let secret (_ : string) = "<redacted>"

let looks_like_iban s =
  let s = String.filter s ~f:(Fn.non Char.is_whitespace) in
  String.length s >= 15
  && String.length s <= 34
  && Char.is_alpha s.[0]
  && Char.is_alpha s.[1]
  && Char.is_digit s.[2]
  && Char.is_digit s.[3]
;;

let destination s =
  if looks_like_iban s
  then iban s
  else if String.mem s '@'
  then email s
  else crypto_address s
;;
