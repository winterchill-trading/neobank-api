open! Core
open Neobank_api

let%test_unit "base32 round trips any bytes" =
  Quickcheck.test String.quickcheck_generator ~sexp_of:String.sexp_of_t ~f:(fun s ->
    [%test_result: string] (ok_exn (Totp.base32_decode (Totp.base32_encode s))) ~expect:s)
;;

(* A JSON object with shuffled keys and random scalar leaves. *)
let json_gen =
  let open Quickcheck.Generator.Let_syntax in
  let scalar =
    Quickcheck.Generator.union
      [ (let%map s = String.gen_with_length 3 Char.gen_alpha in
         `String s)
      ; (let%map i = Int.gen_incl (-1000) 1000 in
         `Int i)
      ; return `Null
      ]
  in
  let%bind keys = List.gen_with_length 4 (String.gen_with_length 2 Char.gen_lowercase) in
  let keys = List.dedup_and_sort keys ~compare:String.compare in
  let%bind values = List.gen_with_length (List.length keys) scalar in
  let%map permutation = List.gen_permutations (List.zip_exn keys values) in
  `Assoc (List.zip_exn keys values), `Assoc permutation
;;

let%test_unit "the request hash ignores key order and spacing" =
  Quickcheck.test
    json_gen
    ~sexp_of:(fun (a, b) ->
      Sexp.List [ Atom (Yojson.Safe.to_string a); Atom (Yojson.Safe.to_string b) ])
    ~f:(fun (a, b) ->
      [%test_result: string]
        (Server.request_hash (Yojson.Safe.pretty_to_string b))
        ~expect:(Server.request_hash (Yojson.Safe.to_string a)))
;;

let%test_unit "the request hash tells different bodies apart" =
  Quickcheck.test
    (Quickcheck.Generator.tuple2 json_gen json_gen)
    ~sexp_of:(fun ((a, _), (b, _)) ->
      Sexp.List [ Atom (Yojson.Safe.to_string a); Atom (Yojson.Safe.to_string b) ])
    ~f:(fun ((a, _), (b, _)) ->
      if not (Yojson.Safe.equal a b)
      then
        assert (
          not
            (String.equal
               (Server.request_hash (Yojson.Safe.to_string a))
               (Server.request_hash (Yojson.Safe.to_string b)))))
;;

let%test_unit "relay backoff grows, never past the cap" =
  let max = Time_ns.Span.of_min 5. in
  Quickcheck.test
    (Int.gen_incl 1 2000)
    ~sexp_of:Int.sexp_of_t
    ~examples:[ 200 ]
    ~f:(fun attempts ->
      let b = Relay.backoff ~attempts ~max in
      assert (Time_ns.Span.( <= ) b max);
      assert (Time_ns.Span.( >= ) b (Time_ns.Span.of_sec 1.));
      assert (Time_ns.Span.( <= ) b (Relay.backoff ~attempts:(attempts + 1) ~max)))
;;

let%test_unit "masked card numbers and IBANs show at most four digits" =
  Quickcheck.test
    (String.gen_with_length 16 Char.gen_digit)
    ~sexp_of:String.sexp_of_t
    ~f:(fun pan ->
      let masked = Redact.pan pan in
      assert (String.count masked ~f:Char.is_digit <= 4);
      let iban = "DE" ^ String.prefix pan 2 ^ pan in
      let masked = Redact.iban iban in
      assert (String.count masked ~f:Char.is_digit <= 4))
;;
