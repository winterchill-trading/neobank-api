open! Core

let alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ234567"

let base32_encode s =
  let buf = Buffer.create ((String.length s * 8 / 5) + 1) in
  let bits = ref 0
  and nbits = ref 0 in
  String.iter s ~f:(fun c ->
    bits := (!bits lsl 8) lor Char.to_int c;
    nbits := !nbits + 8;
    while !nbits >= 5 do
      nbits := !nbits - 5;
      Buffer.add_char buf alphabet.[(!bits lsr !nbits) land 31]
    done);
  if !nbits > 0 then Buffer.add_char buf alphabet.[(!bits lsl (5 - !nbits)) land 31];
  Buffer.contents buf
;;

let base32_decode s =
  let s =
    String.uppercase
      (String.filter s ~f:(fun c -> not (Char.equal c '=' || Char.is_whitespace c)))
  in
  let buf = Buffer.create (String.length s * 5 / 8) in
  let bits = ref 0
  and nbits = ref 0 in
  with_return (fun { return } ->
    String.iter s ~f:(fun c ->
      match String.index alphabet c with
      | None -> return (Or_error.error_s [%message "not base32" ~char:(c : char)])
      | Some v ->
        bits := (!bits lsl 5) lor v land 0xffff;
        nbits := !nbits + 5;
        if !nbits >= 8
        then (
          nbits := !nbits - 8;
          Buffer.add_char buf (Char.of_int_exn ((!bits lsr !nbits) land 0xff))));
    Ok (Buffer.contents buf))
;;

let generate_secret () = Cryptokit.Random.string Cryptokit.Random.secure_rng 20

let otpauth_uri ~issuer ~account ~secret =
  let label = Uri.pct_encode ~component:`Path (issuer ^ ":" ^ account) in
  sprintf
    "otpauth://totp/%s?secret=%s&issuer=%s&algorithm=SHA1&digits=6&period=30"
    label
    (base32_encode secret)
    (Uri.pct_encode ~component:`Query_value issuer)
;;

let digits = 6
let step_seconds = 30
let code ~secret ~time = Payments_core.Sca.Totp.code ~secret ~time ~digits

let step_of time =
  Int63.to_int_exn (Time_ns.to_int63_ns_since_epoch time) / 1_000_000_000 / step_seconds
;;

let constant_time_equal a b =
  String.length a = String.length b
  && String.foldi a ~init:0 ~f:(fun i acc c ->
       acc lor (Char.to_int c lxor Char.to_int b.[i]))
     = 0
;;

let check ~secret ~time ~last_step response =
  let current = step_of time in
  (* Every candidate is computed and compared, so timing does not say which one matched. *)
  let matches =
    List.filter
      [ current - 1; current; current + 1 ]
      ~f:(fun step ->
        let at = Time_ns.of_int_ns_since_epoch (step * step_seconds * 1_000_000_000) in
        constant_time_equal (code ~secret ~time:at) response)
  in
  List.find matches ~f:(fun step ->
    match last_step with
    | Some last -> step > last
    | None -> true)
;;
