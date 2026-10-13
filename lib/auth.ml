open! Core
open Async
open Neobank_api_types
open Model

let ( let* ) = Deferred.Result.( >>= )
let ok = Deferred.Result.return
let fail p = return (Error p)

let validate (r : Api_types.Auth.Signup_request.t) ~now =
  let open Or_error.Let_syntax in
  let check cond msg = if cond then Ok () else Or_error.error_string msg in
  let email = Customer.normalize_email r.email in
  let%bind () =
    check
      (match String.lsplit2 email ~on:'@' with
       | Some (local, domain) ->
         (not (String.is_empty local))
         && String.mem domain '.'
         && String.length email <= 254
         && not (String.exists email ~f:Char.is_whitespace)
       | None -> false)
      "email is not an e-mail address"
  in
  let%bind () =
    check (String.length r.password >= 10) "password must have at least 10 characters"
  in
  let%bind () =
    check (String.length r.password <= 256) "password must have at most 256 characters"
  in
  let%bind () =
    check
      ((not (String.is_empty (String.strip r.given_name)))
       && not (String.is_empty (String.strip r.family_name)))
      "given_name and family_name are required"
  in
  let%bind date_of_birth =
    Or_error.try_with (fun () -> Date.of_string r.date_of_birth)
    |> Or_error.tag ~tag:"date_of_birth must be YYYY-MM-DD"
  in
  let%bind () =
    check
      (let age =
         Date.diff (Time_ns.to_date now ~zone:Time_float.Zone.utc) date_of_birth / 365
       in
       age >= 18 && age < 130)
      "customers must be adults"
  in
  let country s =
    Or_error.try_with (fun () ->
      Kyc_core.Country.of_string (String.uppercase (String.strip s)))
    |> Or_error.tag ~tag:"country codes are two letters (ISO 3166)"
  in
  let%bind nationality = country r.nationality in
  let%bind address_country = country r.address.country in
  let mobile = String.filter r.mobile ~f:(fun c -> not (Char.is_whitespace c)) in
  let%bind () =
    check
      (String.length mobile >= 8
       && String.length mobile <= 16
       && Char.equal mobile.[0] '+'
       && String.for_all (String.drop_prefix mobile 1) ~f:Char.is_digit)
      "mobile must be in E.164 form, e.g. +4930..."
  in
  let%map () =
    check
      (not (String.is_empty (String.strip r.address.line1 ^ String.strip r.address.city)))
      "address line1 and city are required"
  in
  let open Kyc_core in
  ( email
  , { Person.given_name = Pii.create (String.strip r.given_name)
    ; family_name = Pii.create (String.strip r.family_name)
    ; date_of_birth = Pii.create date_of_birth
    ; nationality = Pii.create nationality
    ; residential_address =
        Pii.create
          { Address.line1 = String.strip r.address.line1
          ; line2 = r.address.line2
          ; city = String.strip r.address.city
          ; region = None
          ; postal_code = r.address.postal_code
          ; country = address_country
          }
    ; email = Pii.create email
    ; mobile = Pii.create mobile
    } )
;;

let signup (ctx : Ctx.t) (r : Api_types.Auth.Signup_request.t) =
  let now = Ctx.now ctx in
  let* email, person = return (Problem.of_validation (validate r ~now)) in
  let id = Kyc_core.Customer_id.of_string (Ids.next ctx.ids "cus") in
  let secret = Totp.generate_secret () in
  let iterations = ctx.config.password_iterations in
  let%bind password_hash =
    In_thread.run (fun () -> Password.hash ~iterations r.password)
  in
  let customer : Customer.t =
    { id
    ; email
    ; password_hash = Secret.of_string password_hash
    ; totp_secret = Secret.of_string secret
    ; totp_confirmed = false
    ; last_totp_step = None
    ; provider_ref = None
    ; sca_lock = Payments_core.Sca.Lock.empty
    ; failed_sca_at = []
    ; created_at = now
    }
  in
  let opened : Kyc_core.Event.t =
    { id = Kyc_core.Event_id.of_string (Customer_id.to_string id ^ ":opened")
    ; customer_id = id
    ; at = now
    ; actor = System
    ; note = Some "sign-up"
    ; kind = Opened person
    }
  in
  let* () =
    Ctx.commit
      ctx
      [ Insert_customer customer
      ; Kyc_event opened
      ; Enqueue (Outbox.create ~id:(Ids.next ctx.ids "msg") ~now (Create_customer id))
      ; Ctx.audit
          ctx
          ~actor:("customer:" ^ Customer_id.to_string id)
          ~action:"customer.signup"
          ~object_id:(Customer_id.to_string id)
          ~outcome:"ok"
          ()
      ]
  in
  Metrics.incr ctx.metrics "signups";
  let%bind () = Ctx.settle ctx in
  ok
    { Api_types.Auth.Signup_response.customer_id = Customer_id.to_string id
    ; totp_secret = Totp.base32_encode secret
    ; otpauth_uri = Totp.otpauth_uri ~issuer:ctx.config.totp_issuer ~account:email ~secret
    }
;;

(* A hash to check against when the e-mail is unknown, so an unknown address takes as long
   as a wrong password. *)
let dummy_hash =
  let cache = ref None in
  fun iterations ->
    match !cache with
    | Some (i, h) when i = iterations -> h
    | _ ->
      let h = Password.hash ~iterations "not a password" in
      cache := Some (iterations, h);
      h
;;

let check_factors (ctx : Ctx.t) (c : Customer.t) ~password ~totp =
  let%map password_ok =
    In_thread.run (fun () ->
      Password.verify ~stored:(Secret.reveal c.password_hash) password)
  in
  let step =
    Totp.check
      ~secret:(Secret.reveal c.totp_secret)
      ~time:(Ctx.now ctx)
      ~last_step:c.last_totp_step
      totp
  in
  match password_ok, step with
  | true, Some step -> `Ok { c with last_totp_step = Some step; totp_confirmed = true }
  | _ -> `Wrong
;;

let day = Time_ns.Span.of_day 1.

let failed_sca_24h ctx (c : Customer.t) =
  let since = Time_ns.sub (Ctx.now ctx) day in
  List.count c.failed_sca_at ~f:(fun at -> Time_ns.( > ) at since)
;;

let record_failure ctx (c : Customer.t) =
  let now = Ctx.now ctx in
  let lock, notice = Payments_core.Sca.Lock.record_failure c.sca_lock ~now in
  let since = Time_ns.sub now day in
  ( { c with
      sca_lock = lock
    ; failed_sca_at =
        now :: List.filter c.failed_sca_at ~f:(fun at -> Time_ns.( > ) at since)
    }
  , match notice with
    | Some _ -> `Locked
    | None -> `Not_locked )
;;

let record_success (c : Customer.t) =
  { c with sca_lock = Payments_core.Sca.Lock.record_success c.sca_lock }
;;

let locked_problem =
  Problem.create `Locked "locked" "too many failed attempts; try again later"
;;

let check_not_locked ctx (c : Customer.t) =
  if Payments_core.Sca.Lock.is_locked c.sca_lock ~now:(Ctx.now ctx)
  then fail locked_problem
  else ok ()
;;

let invalid_credentials =
  Problem.unauthorized "invalid_credentials" "the e-mail, password or code is wrong"
;;

let session_view (ctx : Ctx.t) (s : Session.t) ~token : Api_types.Auth.Session.t =
  { token
  ; customer_id = Customer_id.to_string s.customer
  ; idle_timeout_s = Int.of_float (Time_ns.Span.to_sec ctx.config.session_idle)
  ; sca_at = Views.time s.sca_at
  }
;;

(* Checks both factors and records the outcome on the customer. *)
let answer_factors ctx (c : Customer.t) ~password ~totp ~action =
  let* () = check_not_locked ctx c in
  match%bind check_factors ctx c ~password ~totp with
  | `Ok c ->
    let c = record_success c in
    ok c
  | `Wrong ->
    let c', locked = record_failure ctx c in
    Metrics.incr ctx.metrics "sca_failures" ~labels:[ "action", action ];
    let* () =
      Ctx.commit
        ctx
        [ Update_customer c'
        ; Ctx.audit
            ctx
            ~actor:("customer:" ^ Customer_id.to_string c.id)
            ~action
            ~object_id:(Customer_id.to_string c.id)
            ~outcome:"wrong_factor"
            ()
        ]
    in
    (match locked with
     | `Locked -> fail locked_problem
     | `Not_locked -> fail invalid_credentials)
;;

let login (ctx : Ctx.t) (r : Api_types.Auth.Login_request.t) =
  match%bind Ctx.read (Any_store.customer_by_email ctx.store r.email) with
  | Error p -> fail p
  | Ok None ->
    let iterations = ctx.config.password_iterations in
    let%bind (_ : bool) =
      In_thread.run (fun () -> Password.verify ~stored:(dummy_hash iterations) r.password)
    in
    Metrics.incr ctx.metrics "sca_failures" ~labels:[ "action", "login" ];
    fail invalid_credentials
  | Ok (Some c) ->
    let* c = answer_factors ctx c ~password:r.password ~totp:r.totp ~action:"login" in
    let now = Ctx.now ctx in
    let token = Ids.token ctx.ids in
    let session : Session.t =
      { token_hash = Session.hash_token token
      ; customer = c.id
      ; created_at = now
      ; last_activity = now
      ; sca_at = now
      }
    in
    let* () =
      Ctx.commit
        ctx
        [ Update_customer c
        ; Put_session session
        ; Ctx.audit
            ctx
            ~actor:("customer:" ^ Customer_id.to_string c.id)
            ~action:"login"
            ~object_id:(Customer_id.to_string c.id)
            ~outcome:"ok"
            ()
        ]
    in
    Metrics.incr ctx.metrics "logins";
    ok (session_view ctx session ~token)
;;

let expired = Problem.unauthorized "session_expired" "the session has ended; log in again"

let authenticate (ctx : Ctx.t) ~token =
  let token_hash = Session.hash_token token in
  match%bind Ctx.read (Any_store.session ctx.store ~token_hash) with
  | Error p -> fail p
  | Ok None -> fail (Problem.unauthorized "no_session" "missing or unknown session token")
  | Ok (Some s) ->
    let now = Ctx.now ctx in
    if Time_ns.Span.( > ) (Time_ns.diff now s.last_activity) ctx.config.session_idle
       || Time_ns.Span.( > ) (Time_ns.diff now s.created_at) ctx.config.session_max
    then (
      let%bind (_ : (unit, Problem.t) Result.t) =
        Ctx.commit ctx [ Delete_session { token_hash } ]
      in
      fail expired)
    else if Time_ns.Span.( < )
              (Time_ns.diff now s.last_activity)
              (Time_ns.Span.of_sec 10.)
    then ok s
    else (
      (* Touch at most every 10 s, so reads do not write on every request. *)
      let s = { s with last_activity = now } in
      let* () = Ctx.commit ctx [ Put_session s ] in
      ok s)
;;

let step_up (ctx : Ctx.t) (s : Session.t) (r : Api_types.Auth.Step_up_request.t) =
  let* c = Ctx.find ~what:"customer" (Any_store.customer ctx.store s.customer) in
  let* c = answer_factors ctx c ~password:r.password ~totp:r.totp ~action:"step_up" in
  let now = Ctx.now ctx in
  let s = { s with sca_at = now; last_activity = now } in
  let* () = Ctx.commit ctx [ Update_customer c; Put_session s ] in
  ok (session_view ctx s ~token:"")
;;

let logout (ctx : Ctx.t) (s : Session.t) =
  Ctx.commit ctx [ Delete_session { token_hash = s.token_hash } ]
;;

let sca_is_fresh (ctx : Ctx.t) (s : Session.t) =
  Time_ns.Span.( <= ) (Time_ns.diff (Ctx.now ctx) s.sca_at) ctx.config.sca_fresh_for
;;

let require_fresh_sca ctx s =
  if sca_is_fresh ctx s
  then ok ()
  else
    fail
      (Problem.forbidden
         "sca_required"
         "confirm with your password and authenticator code first (POST /auth/step-up)")
;;
