open! Core
open Neobank_api

(* RFC 6238 appendix B, SHA-1 key "12345678901234567890", truncated to 6 digits. *)
let secret = "12345678901234567890"
let at s = Time_ns.of_int_ns_since_epoch (s * 1_000_000_000)

let%expect_test "RFC 6238 vectors (6 digits)" =
  List.iter [ 59; 1111111109; 1111111111; 1234567890; 2000000000 ] ~f:(fun s ->
    print_endline (Totp.code ~secret ~time:(at s)));
  [%expect {|
    287082
    081804
    050471
    005924
    279037
    |}]
;;

let%expect_test "base32 round trip and the RFC 4648 vectors" =
  List.iter [ ""; "f"; "fo"; "foo"; "foob"; "fooba"; "foobar" ] ~f:(fun s ->
    let e = Totp.base32_encode s in
    printf "%S -> %s -> %S\n" s e (ok_exn (Totp.base32_decode e)));
  [%expect
    {|
    "" ->  -> ""
    "f" -> MY -> "f"
    "fo" -> MZXQ -> "fo"
    "foo" -> MZXW6 -> "foo"
    "foob" -> MZXW6YQ -> "foob"
    "fooba" -> MZXW6YTB -> "fooba"
    "foobar" -> MZXW6YTBOI -> "foobar"
    |}];
  print_s [%sexp (Totp.base32_decode "MZ!" : string Or_error.t)];
  [%expect {| (Error ("not base32" (char !))) |}]
;;

let%expect_test "a step is accepted once, with one step of drift either side" =
  let now = at 1_000_000_020 in
  let code_at s = Totp.code ~secret ~time:(at s) in
  let check ?last_step c = Totp.check ~secret ~time:now ~last_step c in
  let current = check (code_at 1_000_000_020) in
  print_s
    [%message
      ""
        (current : int option)
        ~previous_step:(check (code_at 1_000_000_000) : int option)
        ~next_step:(check (code_at 1_000_000_050) : int option)
        ~two_steps_old:(check (code_at 999_999_960) : int option)
        ~replayed:(check ?last_step:current (code_at 1_000_000_020) : int option)
        ~wrong:(check "000000" : int option)];
  [%expect
    {|
    ((current (33333334)) (previous_step (33333333)) (next_step (33333335))
     (two_steps_old ()) (replayed ()) (wrong ()))
    |}]
;;

let%expect_test "otpauth URI" =
  print_endline (Totp.otpauth_uri ~issuer:"Neobank Demo" ~account:"a@example.com" ~secret);
  [%expect
    {| otpauth://totp/Neobank%20Demo:a@example.com?secret=GEZDGNBVGY3TQOJQGEZDGNBVGY3TQOJQ&issuer=Neobank%20Demo&algorithm=SHA1&digits=6&period=30 |}]
;;
