open! Core
open Async
open Harness

let login t ~password ~code =
  post
    t
    "/auth/login"
    ~body:
      (`Assoc
        [ "email", `String "customer@example.com"
        ; "password", `String password
        ; "totp", `String code
        ])
;;

let%expect_test "sign-up checks, login failures and the lock" =
  let%bind () =
    with_api (fun t ->
      let%bind r = post t "/auth/signup" ~body:(`Assoc [ "email", `String "x" ]) in
      show "missing fields" r ~fields:[ "error" ];
      let bad field value =
        match signup_body "customer@example.com" with
        | `Assoc fields ->
          `Assoc
            (List.map fields ~f:(fun (k, v) ->
               if String.equal k field then k, value else k, v))
        | j -> j
      in
      let%bind r = post t "/auth/signup" ~body:(bad "email" (`String "not-an-address")) in
      show "bad e-mail" r ~fields:[ "message" ];
      let%bind r = post t "/auth/signup" ~body:(bad "password" (`String "short")) in
      show "short password" r ~fields:[ "message" ];
      let%bind r =
        post t "/auth/signup" ~body:(bad "date_of_birth" (`String "2015-01-01"))
      in
      show "minor" r ~fields:[ "message" ];
      let%bind r = post t "/auth/signup" ~body:(bad "mobile" (`String "030 1234")) in
      show "bad mobile" r ~fields:[ "message" ];
      let%bind r = post t "/auth/signup" ~body:(signup_body "customer@example.com") in
      show "signed up" r ~fields:[ "customer_id" ];
      let secret = str r.json "totp_secret" in
      let%bind r = post t "/auth/signup" ~body:(signup_body "Customer@Example.com") in
      show "same e-mail again" r ~fields:[ "error" ];
      let%bind r = login t ~password:"correct horse battery" ~code:"000000" in
      show "wrong code" r ~fields:[ "error"; "message" ];
      let%bind r =
        post
          t
          "/auth/login"
          ~body:
            (`Assoc
              [ "email", `String "nobody@example.com"
              ; "password", `String "correct horse battery"
              ; "totp", `String "000000"
              ])
      in
      show "unknown e-mail" r ~fields:[ "error"; "message" ];
      let%bind () =
        Deferred.List.iter ~how:`Sequential (List.range 0 4) ~f:(fun i ->
          let%map r =
            login t ~password:(sprintf "wrong %d password" i) ~code:(totp t secret)
          in
          show "wrong password" r ~fields:[ "error" ])
      in
      next_step t;
      let%bind r = login t ~password:"correct horse battery" ~code:(totp t secret) in
      show "locked, even with the right answers" r ~fields:[ "error" ];
      advance t (Time_ns.Span.of_min 31.);
      let%bind r = login t ~password:"correct horse battery" ~code:(totp t secret) in
      show "after the lock" r ~fields:[ "customer_id" ];
      t.token <- Some (str r.json "token");
      let%bind r = login t ~password:"correct horse battery" ~code:(totp t secret) in
      show "the same code again" r ~fields:[ "error" ];
      let%bind r = post t "/auth/logout" in
      show "logout" r;
      let%bind r = get t "/me" in
      show "after logout" r ~fields:[ "error" ];
      return ())
  in
  [%expect
    {|
    missing fields: 400 error=invalid_request
    bad e-mail: 400 message=email is not an e-mail address
    short password: 400 message=password must have at least 10 characters
    minor: 400 message=customers must be adults
    bad mobile: 400 message=mobile must be in E.164 form, e.g. +4930...
    signed up: 201 customer_id=cus-000001
    same e-mail again: 409 error=email_taken
    wrong code: 401 error=invalid_credentials message=the e-mail, password or code is wrong
    unknown e-mail: 401 error=invalid_credentials message=the e-mail, password or code is wrong
    wrong password: 401 error=invalid_credentials
    wrong password: 401 error=invalid_credentials
    wrong password: 401 error=invalid_credentials
    wrong password: 423 error=locked
    locked, even with the right answers: 423 error=locked
    after the lock: 200 customer_id=cus-000001
    the same code again: 401 error=invalid_credentials
    logout: 204
    after logout: 401 error=no_session
    |}];
  return ()
;;
