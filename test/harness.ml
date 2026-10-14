(* An API on port 0 over the fake provider, a synthetic clock and counted ids, with a
   small HTTP client: every run of a scenario prints the same thing. *)

open! Core
open Async
open Neobank_api
module Time_source = Async_kernel.Time_source

type t =
  { ctx : Ctx.t
  ; clock : Time_source.Read_write.t
  ; fake : Banking_provider.Fake_provider.t
  ; port : int
  ; mutable token : string option
  ; mutable keys : int
  ; server : Server.t
  ; http : (Socket.Address.Inet.t, int) Cohttp_async.Server.t
  }

let start_time = Time_ns.of_string_with_utc_offset "2026-10-14 09:00:00Z"
let ops_token = "ops-test-token"

let with_api ?(store = Any_store.pack (module Store_memory) (Store_memory.create ())) f =
  Log.Global.set_output [];
  let dir = Filename_unix.temp_dir "neobank-api" "test" in
  let clock = Time_source.create ~now:start_time () in
  let bank =
    Bank.fake
      ~config:{ Banking_provider.Fake_provider.Config.default with auto_enrich = true }
      ~time_source:(Time_source.read_only clock)
      ~state_dir:dir
      ()
    |> ok_exn
  in
  let ctx =
    Ctx.create
      ~ids:(Ids.counter ())
      ~time_source:(Time_source.read_only clock)
      ~config:
        { Ctx.Config.default with password_iterations = 1000; ops_token = Some ops_token }
      ~store
      ~bank
      ()
  in
  let stop_events = Events.start ctx in
  let stop = Ivar.create () in
  Relay.start ctx ~stop:(Ivar.read stop);
  let server = Server.create ctx in
  let%bind http = Server.start server ~port:0 in
  let t =
    { ctx
    ; clock
    ; fake = Option.value_exn (Bank.as_fake bank)
    ; port = Cohttp_async.Server.listening_on http
    ; token = None
    ; keys = 0
    ; server
    ; http
    }
  in
  Monitor.protect
    (fun () -> f t)
    ~finally:(fun () ->
      Ivar.fill_if_empty stop ();
      stop_events ();
      Server.close server;
      let%bind () = Cohttp_async.Server.close http in
      Bank.close bank;
      let%map (_ : Core_unix.Exit_or_signal.t) =
        In_thread.run (fun () ->
          Core_unix.system (sprintf "rm -rf %s" (Filename.quote dir)))
      in
      ())
;;

(* The ledger restamps entries strictly increasing, so a frozen clock would collide; every
   request moves the clock a second. *)
let advance t span =
  Time_source.advance_directly t.clock ~to_:(Time_ns.add (Time_source.now t.clock) span)
;;

let next_key t =
  t.keys <- t.keys + 1;
  sprintf "key-%04d" t.keys
;;

type response =
  { status : int
  ; json : Yojson.Safe.t
  ; headers : (string * string) list
  }

let request ?key ?(token = `Session) ?body t meth path =
  advance t (Time_ns.Span.of_sec 1.);
  let uri = Uri.make ~scheme:"http" ~host:"127.0.0.1" ~port:t.port ~path () in
  let uri =
    match String.lsplit2 path ~on:'?' with
    | Some (p, q) -> Uri.with_query (Uri.with_path uri p) (Uri.query_of_encoded q)
    | None -> uri
  in
  let auth =
    match token with
    | `Session -> Option.map t.token ~f:(fun tok -> "authorization", "Bearer " ^ tok)
    | `Ops -> Some ("authorization", "Bearer " ^ ops_token)
    | `None -> None
  in
  let key =
    match key, meth with
    | Some (`Key k), _ -> Some k
    | Some `No_key, _ -> None
    | None, `POST -> Some (next_key t)
    | None, _ -> None
  in
  let headers =
    Cohttp.Header.of_list
      (("content-type", "application/json")
       :: (Option.to_list auth
           @ Option.to_list (Option.map key ~f:(fun k -> "idempotency-key", k))))
  in
  let body_text = Option.value_map body ~default:"" ~f:Yojson.Safe.to_string in
  let%bind resp, rbody =
    Cohttp_async.Client.call
      ~headers
      ~body:(Cohttp_async.Body.of_string body_text)
      meth
      uri
  in
  let%map text = Cohttp_async.Body.to_string rbody in
  { status = Cohttp.Code.code_of_status (Cohttp.Response.status resp)
  ; json =
      (if String.is_empty text
       then `Null
       else (
         try Yojson.Safe.from_string text with
         | _ -> `String text))
  ; headers = Cohttp.Header.to_list (Cohttp.Response.headers resp)
  }
;;

let get ?token t path = request ?token t `GET path
let post ?key ?token ?body t path = request ?key ?token ?body t `POST path

let rec field json path =
  match path with
  | [] -> json
  | k :: rest ->
    (match json with
     | `Assoc l ->
       field (Option.value (List.Assoc.find l ~equal:String.equal k) ~default:`Null) rest
     | `List l ->
       (match Int.of_string_opt k with
        | Some i -> field (Option.value (List.nth l i) ~default:`Null) rest
        | None -> `Null)
     | _ -> `Null)
;;

let str json path =
  match field json (String.split path ~on:'.') with
  | `String s -> s
  | `Null -> "<none>"
  | j -> Yojson.Safe.to_string j
;;

(* Prints the status and the named fields. *)
let show ?(fields = []) label (r : response) =
  let parts = List.map fields ~f:(fun f -> sprintf "%s=%s" f (str r.json f)) in
  print_endline (String.concat ~sep:" " (sprintf "%s: %d" label r.status :: parts))
;;

let totp t secret =
  Totp.code ~secret:(Totp.base32_decode secret |> ok_exn) ~time:(Time_source.now t.clock)
;;

(* A fresh 30-second step, so the next code has not been used. *)
let next_step t = advance t (Time_ns.Span.of_sec 30.)
let person = [ "given_name", `String "Test"; "family_name", `String "Customer" ]

let signup_body email =
  `Assoc
    ([ "email", `String email; "password", `String "correct horse battery" ]
     @ person
     @ [ "date_of_birth", `String "1990-01-01"
       ; "nationality", `String "DE"
       ; "mobile", `String "+4930000000"
       ; ( "address"
         , `Assoc
             [ "line1", `String "1 Test Street"
             ; "city", `String "Berlin"
             ; "postal_code", `String "10115"
             ; "country", `String "DE"
             ] )
       ])
;;

let amount a c = `Assoc [ "amount", `String a; "currency", `String c ]

(* Sign-up to an approved customer with funded accounts; returns the TOTP secret. *)
let onboard ?(email = "customer@example.com") ?(deposit = "1000.00") t =
  let%bind r = post t "/auth/signup" ~body:(signup_body email) in
  let secret = str r.json "totp_secret" in
  next_step t;
  let%bind r =
    post
      t
      "/auth/login"
      ~body:
        (`Assoc
          [ "email", `String email
          ; "password", `String "correct horse battery"
          ; "totp", `String (totp t secret)
          ])
  in
  t.token <- Some (str r.json "token");
  let code = `String Banking_provider.Fake_provider.sandbox_code in
  let%bind () =
    Deferred.List.iter
      ~how:`Sequential
      [ "email"; "mobile"; "identity_document"; "liveness" ]
      ~f:(fun step ->
        let%bind (_ : response) =
          post t "/kyc/start" ~body:(`Assoc [ "step", `String step ])
        in
        match step with
        | "email" | "mobile" ->
          let%map (_ : response) =
            post t "/kyc/complete" ~body:(`Assoc [ "step", `String step; "code", code ])
          in
          ()
        | _ -> return ())
  in
  let%bind (_ : response) =
    post t "/sandbox/kyc-decision" ~body:(`Assoc [ "decision", `String "approve" ])
  in
  let%bind accounts = get t "/accounts" in
  let id currency =
    match accounts.json with
    | `List l ->
      List.find_map_exn l ~f:(fun a ->
        if String.equal (str a "currency") currency then Some (str a "id") else None)
    | _ -> failwith "no accounts"
  in
  let eur = id "EUR" in
  let%map (_ : response) =
    post
      t
      "/sandbox/deposit"
      ~body:(`Assoc [ "account_id", `String eur; "amount", amount deposit "EUR" ])
  in
  secret, eur, id "BTC"
;;
