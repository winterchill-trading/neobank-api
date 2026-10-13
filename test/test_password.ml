open! Core
open Neobank_api

let hex s = Cryptokit.transform_string (Cryptokit.Hexa.encode ()) s

(* RFC 7914 section 11 vectors for PBKDF2-HMAC-SHA256, also printed by Python's
   hashlib.pbkdf2_hmac. *)
let%expect_test "PBKDF2-HMAC-SHA256 vectors" =
  print_endline
    (hex
       (Password.pbkdf2_sha256 ~password:"passwd" ~salt:"salt" ~iterations:1 ~length:64));
  print_endline
    (hex
       (Password.pbkdf2_sha256
          ~password:"Password"
          ~salt:"NaCl"
          ~iterations:80000
          ~length:64));
  [%expect
    {|
    55ac046e56e3089fec1691c22544b605f94185216dde0465e68b9d57c20dacbc49ca9cccf179b645991664b39d77ef317c71b845b1e30bd509112041d3a19783
    4ddcd8f60b98be21830cee5ef22701f9641a4418d04c0414aeff08876b34ab56a1d425a1225833549adb841b51c9b3176a272bdebba1d078478f62b397f33c8d
    |}]
;;

let%expect_test "hash and verify" =
  let stored = Password.hash ~iterations:1000 "correct horse" in
  print_s
    [%message
      ""
        ~prefix:(String.prefix stored 19 : string)
        ~right:(Password.verify ~stored "correct horse" : bool)
        ~wrong:(Password.verify ~stored "correct horsE" : bool)
        ~malformed:(Password.verify ~stored:"md5$abc" "x" : bool)
        ~salted:
          (not (String.equal stored (Password.hash ~iterations:1000 "correct horse"))
           : bool)];
  [%expect
    {|
    ((prefix pbkdf2-sha256$1000$) (right true) (wrong false) (malformed false)
     (salted true))
    |}]
;;
