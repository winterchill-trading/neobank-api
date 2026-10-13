open! Core

let hmac ~key message = Cryptokit.hash_string (Cryptokit.MAC.hmac_sha256 key) message

let pbkdf2_sha256 ~password ~salt ~iterations ~length =
  if iterations < 1 then invalid_arg "Password.pbkdf2_sha256: iterations";
  let hlen = 32 in
  let blocks = (length + hlen - 1) / hlen in
  let block i =
    let int_be =
      String.init 4 ~f:(fun k -> Char.of_int_exn ((i lsr (8 * (3 - k))) land 0xff))
    in
    let u = ref (hmac ~key:password (salt ^ int_be)) in
    let acc = Bytes.of_string !u in
    for _ = 2 to iterations do
      u := hmac ~key:password !u;
      String.iteri !u ~f:(fun j c ->
        Bytes.set
          acc
          j
          (Char.of_int_exn (Char.to_int (Bytes.get acc j) lxor Char.to_int c)))
    done;
    Bytes.to_string acc
  in
  String.concat (List.init blocks ~f:(fun i -> block (i + 1)))
  |> Fn.flip String.prefix length
;;

(* OWASP Password Storage Cheat Sheet value for PBKDF2-HMAC-SHA256. *)
let default_iterations = 600_000
let hex s = Cryptokit.transform_string (Cryptokit.Hexa.encode ()) s

let unhex s =
  Option.try_with (fun () -> Cryptokit.transform_string (Cryptokit.Hexa.decode ()) s)
;;

let hash ?(iterations = default_iterations) password =
  let salt = Cryptokit.Random.string Cryptokit.Random.secure_rng 16 in
  let key = pbkdf2_sha256 ~password ~salt ~iterations ~length:32 in
  sprintf "pbkdf2-sha256$%d$%s$%s" iterations (hex salt) (hex key)
;;

let constant_time_equal a b =
  String.length a = String.length b
  &&
  let diff = ref 0 in
  String.iteri a ~f:(fun i c -> diff := !diff lor (Char.to_int c lxor Char.to_int b.[i]));
  !diff = 0
;;

let verify ~stored password =
  match String.split stored ~on:'$' with
  | [ "pbkdf2-sha256"; iterations; salt; key ] ->
    (match Int.of_string_opt iterations, unhex salt, unhex key with
     | Some iterations, Some salt, Some key when iterations >= 1 ->
       let derived =
         pbkdf2_sha256 ~password ~salt ~iterations ~length:(String.length key)
       in
       constant_time_equal derived key
     | _ -> false)
  | _ -> false
;;
