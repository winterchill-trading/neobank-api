open! Core
open Async
open Harness

let password = `String "correct horse battery"

let scenario t =
  let%bind secret, eur, btc = onboard t in
  let%bind r = get t "/me" in
  show "me" r ~fields:[ "kyc.status"; "kyc.tier"; "accounts"; "sca_fresh" ];
  let%bind r = get t ("/accounts/" ^ eur) in
  show "eur" r ~fields:[ "iban"; "balance.available.amount" ];
  (* Swap *)
  let%bind r =
    post
      t
      "/swaps/quote"
      ~body:
        (`Assoc
          [ "from_account_id", `String eur
          ; "to_account_id", `String btc
          ; "sell", amount "100.00" "EUR"
          ])
  in
  show "swap quote" r ~fields:[ "status"; "sell.amount"; "buy.amount"; "price" ];
  let swap = str r.json "id" in
  let%bind r = post t ("/swaps/" ^ swap ^ "/execute") in
  show "swap execute" r ~fields:[ "status"; "provider_sca.factors" ];
  let%bind r =
    post
      t
      ("/sca/" ^ str r.json "provider_sca.challenge_id")
      ~body:(`Assoc [ "code", `String "123456" ])
  in
  show "provider code" r ~fields:[ "status" ];
  let%bind r = get t ("/swaps/" ^ swap) in
  show "swap" r ~fields:[ "status" ];
  (* A SEPA payment *)
  let%bind r =
    post
      t
      "/beneficiaries"
      ~body:
        (`Assoc
          [ "name", `String "Payee"
          ; "rail", `String "sepa"
          ; "destination", `String "FR14 2004 1010 0505 0001 3M02 606"
          ])
  in
  show "beneficiary" r ~fields:[ "destination"; "rails"; "trusted" ];
  let payee = str r.json "id" in
  let pay ?key amount_ =
    post
      t
      ?key
      "/transfers"
      ~body:
        (`Assoc
          [ "account_id", `String eur
          ; "beneficiary_id", `String payee
          ; "amount", amount amount_ "EUR"
          ; "reference", `String "Invoice 42"
          ])
  in
  let%bind r = pay ~key:(`Key "pay-1") "30.00" in
  show "transfer" r ~fields:[ "status"; "fee.amount"; "sca.factors"; "sca.attempts_left" ];
  let transfer = str r.json "id" in
  let%bind again = pay ~key:(`Key "pay-1") "30.00" in
  show "same key, same body" again ~fields:[ "id" ];
  print_s
    [%message
      ""
        ~same_body:(Yojson.Safe.equal again.json r.json : bool)
        ~replayed:
          (List.Assoc.find again.headers "idempotent-replayed" ~equal:String.equal
           : string option)];
  let%bind r = pay ~key:(`Key "pay-1") "31.00" in
  show "same key, other body" r ~fields:[ "error" ];
  let%bind r = post t "/transfers" ~key:`No_key ~body:(`Assoc []) in
  show "no key" r ~fields:[ "error" ];
  next_step t;
  let confirm body = post t ("/transfers/" ^ transfer ^ "/confirm-sca") ~body in
  let%bind r =
    confirm
      (`Assoc [ "password", `String "wrong password!"; "code", `String (totp t secret) ])
  in
  show "wrong password" r ~fields:[ "error"; "message"; "challenge.attempts_left" ];
  let%bind r = confirm (`Assoc [ "password", password; "code", `String "000000" ]) in
  show "wrong code" r ~fields:[ "error"; "message"; "challenge.attempts_left" ];
  let%bind r =
    confirm (`Assoc [ "password", password; "code", `String (totp t secret) ])
  in
  show "confirmed" r ~fields:[ "status"; "provider_sca.factors" ];
  let%bind r = confirm (`Assoc [ "code", `String "123456" ]) in
  show "provider code" r ~fields:[ "status" ];
  let%bind r = get t ("/accounts/" ^ eur) in
  show "eur" r ~fields:[ "balance.available.amount" ];
  (* Webhook redelivery: every event again, out of order, changes nothing. *)
  let deliveries = Banking_provider.Fake_provider.webhook_deliveries t.fake in
  let%bind results =
    Deferred.List.map
      ~how:`Sequential
      (List.rev deliveries)
      ~f:(fun (d : Banking_provider.Webhook.Request.t) ->
        let%map r =
          Cohttp_async.Client.post
            ~headers:(Cohttp.Header.of_list d.headers)
            ~body:(Cohttp_async.Body.of_string d.body)
            (Uri.make
               ~scheme:"http"
               ~host:"127.0.0.1"
               ~port:t.port
               ~path:"/webhooks/provider"
               ())
        in
        r)
  in
  let%bind applied =
    Deferred.List.map ~how:`Sequential results ~f:(fun (resp, body) ->
      let%map body = Cohttp_async.Body.to_string body in
      Cohttp.Code.code_of_status (Cohttp.Response.status resp), body)
  in
  print_s
    [%message
      "redelivered"
        ~deliveries:(List.length deliveries : int)
        ~all_ok:(List.for_all applied ~f:(fun (s, _) -> s = 200) : bool)
        ~newly_applied:
          (List.count applied ~f:(fun (_, b) ->
             not (String.is_substring b ~substring:"\"applied\":0"))
           : int)];
  let%bind r =
    Cohttp_async.Client.post
      ~headers:(Cohttp.Header.of_list [ "x-signature", "t=0,v1=00" ])
      ~body:(Cohttp_async.Body.of_string "{}")
      (Uri.make
         ~scheme:"http"
         ~host:"127.0.0.1"
         ~port:t.port
         ~path:"/webhooks/provider"
         ())
  in
  print_s
    [%message
      "forged webhook"
        ~status:(Cohttp.Code.code_of_status (Cohttp.Response.status (fst r)) : int)];
  (* Review and refusal by risk-rules (the customer is verified: review above 5000 EUR,
     refuse above 15000 EUR). *)
  let%bind (_ : response) =
    post
      t
      "/sandbox/deposit"
      ~body:(`Assoc [ "account_id", `String eur; "amount", amount "20000.00" "EUR" ])
  in
  (* A day later the payee is no longer new (risk-rules' 24-hour cooldown), and the
     session has long ended. *)
  advance t (Time_ns.Span.of_hr 25.);
  let%bind r = get t "/me" in
  (* [session_expired], or [no_session] once the hourly pruning removed it. *)
  show "after a day" r;
  next_step t;
  let%bind r =
    post
      t
      "/auth/login"
      ~body:
        (`Assoc
          [ "email", `String "customer@example.com"
          ; "password", password
          ; "totp", `String (totp t secret)
          ])
  in
  t.token <- Some (str r.json "token");
  let%bind r = pay "16000.00" in
  show "above the cap" r ~fields:[ "error"; "message" ];
  let%bind r = pay "6000.00" in
  show "held" r ~fields:[ "status"; "under_review"; "message"; "sca" ];
  let held = str r.json "id" in
  let%bind r = get t "/ops/review" ~token:`None in
  show "ops without token" r ~fields:[ "error" ];
  let%bind r = get t "/ops/review" ~token:`Ops in
  show "review queue" r ~fields:[ "0.reference"; "0.severity"; "0.status" ];
  let review = str r.json "0.id" in
  let decide operator =
    post
      t
      ("/ops/review/" ^ review)
      ~token:`Ops
      ~body:(`Assoc [ "verdict", `String "approve"; "operator", `String operator ])
  in
  let%bind r = decide "op-1" in
  show "first approval" r ~fields:[ "status"; "operator" ];
  let%bind r = get t ("/transfers/" ^ held) in
  show "still held" r ~fields:[ "status"; "message" ];
  let%bind r = decide "op-1" in
  show "same operator again" r ~fields:[ "error" ];
  let%bind r = decide "op-2" in
  show "approved" r ~fields:[ "status"; "operator" ];
  let%bind r = get t ("/transfers/" ^ held) in
  show "released" r ~fields:[ "status"; "under_review"; "sca.factors" ];
  next_step t;
  let%bind (_ : response) =
    post
      t
      ("/transfers/" ^ held ^ "/confirm-sca")
      ~body:(`Assoc [ "password", password; "code", `String (totp t secret) ])
  in
  let%bind r =
    post
      t
      ("/transfers/" ^ held ^ "/confirm-sca")
      ~body:(`Assoc [ "code", `String "123456" ])
  in
  show "held payment" r ~fields:[ "status" ];
  (* Cards *)
  let%bind r =
    post t "/cards" ~key:(`Key "card-1") ~body:(`Assoc [ "account_id", `String eur ])
  in
  show "card" r ~fields:[ "status"; "kind"; "masked_pan"; "limits.online" ];
  let card = str r.json "id" in
  let%bind r =
    post t "/cards" ~key:(`Key "card-1") ~body:(`Assoc [ "account_id", `String eur ])
  in
  show "same card request" r ~fields:[ "id" ];
  let%bind r = post t ("/cards/" ^ card ^ "/freeze") in
  show "freeze" r ~fields:[ "status" ];
  let auth () =
    post
      t
      "/sandbox/card-auth"
      ~body:
        (`Assoc
          [ "card_id", `String card
          ; "merchant", `String "Corner Shop"
          ; "mcc", `Int 5411
          ; "country", `String "DE"
          ; "amount", amount "12.50" "EUR"
          ])
  in
  let%bind r = auth () in
  show "purchase while frozen" r ~fields:[ "response_code"; "approved" ];
  let%bind r = post t ("/cards/" ^ card ^ "/unfreeze") in
  show "unfreeze" r ~fields:[ "status" ];
  let%bind r = auth () in
  show "purchase" r ~fields:[ "response_code"; "approved" ];
  let%bind r = get t ("/cards/" ^ card ^ "/display") in
  show "display" r ~fields:[ "provider"; "card_id"; "ui_secret" ];
  let%bind r =
    post
      t
      ("/cards/" ^ card ^ "/consent")
      ~body:(`Assoc [ "challenge_id", `String "from-the-browser" ])
  in
  show "consent" r ~fields:[ "factors" ];
  let%bind r = post t ("/cards/" ^ card ^ "/pan-token") in
  show "pan token" r ~fields:[ "error"; "challenge.factors" ];
  let%bind (_ : response) =
    post
      t
      ("/sca/" ^ str r.json "challenge.challenge_id")
      ~body:(`Assoc [ "code", `String "123456" ])
  in
  let%bind r = post t ("/cards/" ^ card ^ "/pan-token") in
  print_s
    [%message
      "pan token"
        ~status:(r.status : int)
        ~has_token:(String.is_prefix (str r.json "token") ~prefix:"<" |> not : bool)];
  (* SCA freshness: the session stays alive, but 6 minutes after the last SCA a card limit
     change needs a step-up. *)
  advance t (Time_ns.Span.of_min 4.);
  let%bind (_ : response) = get t "/me" in
  advance t (Time_ns.Span.of_min 2.);
  let limits = `Assoc [ "daily", amount "200.00" "EUR"; "online", `Bool false ] in
  let%bind r = post t ("/cards/" ^ card ^ "/limits") ~body:limits in
  show "limits, stale SCA" r ~fields:[ "error" ];
  next_step t;
  let%bind (_ : response) = get t "/me" in
  let%bind r =
    post
      t
      "/auth/step-up"
      ~body:(`Assoc [ "password", password; "totp", `String (totp t secret) ])
  in
  show "step-up" r ~fields:[ "customer_id" ];
  let%bind r = post t ("/cards/" ^ card ^ "/limits") ~body:limits in
  show "limits" r ~fields:[ "limits.daily.amount"; "limits.online" ];
  (* Standing order *)
  let%bind r =
    post
      t
      "/standing-orders"
      ~body:
        (`Assoc
          [ "account_id", `String eur
          ; "beneficiary_id", `String payee
          ; "amount", amount "10.00" "EUR"
          ; "schedule", `String "monthly:1"
          ; "start", `String "2026-11-01"
          ; "reference", `String "Rent"
          ])
  in
  show "standing order" r ~fields:[ "status"; "next_run"; "schedule" ];
  let%bind (_ : response) =
    post
      t
      ("/sca/" ^ str r.json "provider_sca.challenge_id")
      ~body:(`Assoc [ "code", `String "123456" ])
  in
  let%bind r = get t "/standing-orders" in
  show "standing orders" r ~fields:[ "0.status" ];
  (* The first run, on 1 November. *)
  Async_kernel.Time_source.advance_directly
    t.clock
    ~to_:(Time_ns.of_string_with_utc_offset "2026-11-01 10:00:00Z");
  let%bind runs = Banking_provider.Fake_provider.run_standing_orders t.fake >>| ok_exn in
  let%bind () = Neobank_api.Ctx.settle t.ctx in
  print_s [%message "runs" ~transfers:(List.length runs : int)];
  next_step t;
  let%bind r =
    post
      t
      "/auth/login"
      ~body:
        (`Assoc
          [ "email", `String "customer@example.com"
          ; "password", password
          ; "totp", `String (totp t secret)
          ])
  in
  t.token <- Some (str r.json "token");
  let%bind r = get t "/standing-orders" in
  show "standing orders" r ~fields:[ "0.status"; "0.runs"; "0.next_run" ];
  (* Statement *)
  let%bind r =
    get t ("/accounts/" ^ eur ^ "/statement?from=2026-10-01&until=2026-11-02")
  in
  show "statement" r ~fields:[ "opening.amount"; "closing.amount" ];
  (match field r.json [ "lines" ] with
   | `List lines ->
     List.iter lines ~f:(fun l ->
       printf
         "  %s %s %s\n"
         (str l "amount.amount")
         (str l "code")
         (str l "balance.amount"))
   | _ -> ());
  let%bind r = get t "/ops/audit" ~token:`Ops in
  show "audit" r ~fields:[ "chain" ];
  let%bind r = get t "/ops/ledger" ~token:`Ops in
  show "ledger" r ~fields:[ "balanced"; "trial_balance" ];
  let%bind r = get t "/transfers" in
  (match r.json with
   | `List l ->
     List.iter l ~f:(fun x ->
       printf
         "  %s %s %s %s\n"
         (str x "direction")
         (str x "amount.amount")
         (str x "rail")
         (str x "status"))
   | _ -> ());
  return ()
;;

let%expect_test "the demo walk on the memory store" =
  let%bind () = with_api scenario in
  [%expect
    {|
    me: 200 kyc.status=active kyc.tier=T2 accounts=6 sca_fresh=true
    eur: 200 iban=DE97500105170000000001 balance.available.amount=1000.00
    swap quote: 201 status=quoted sell.amount=100.00 buy.amount=0.00166666 price=0.0000166666
    swap execute: 200 status=awaiting_sca provider_sca.factors=["otp"]
    provider code: 200 status=accepted
    swap: 200 status=executed
    beneficiary: 201 destination=FR** **** 2606 rails=["sepa","sepa_instant"] trusted=false
    transfer: 201 status=awaiting_sca fee.amount=0.00 sca.factors=["totp","password"] sca.attempts_left=5
    same key, same body: 201 id=tr-000007
    ((same_body true) (replayed (true)))
    same key, other body: 422 error=idempotency_key_reused
    no key: 400 error=idempotency_key_required
    wrong password: 422 error=sca_failed message=the code or password is wrong; 4 attempts left challenge.attempts_left=4
    wrong code: 422 error=sca_failed message=the code or password is wrong; 3 attempts left challenge.attempts_left=3
    confirmed: 200 status=awaiting_sca provider_sca.factors=["otp"]
    provider code: 200 status=settled
    eur: 200 balance.available.amount=870.00
    (redelivered (deliveries 27) (all_ok true) (newly_applied 0))
    ("forged webhook" (status 401))
    after a day: 401
    above the cap: 422 error=payment_refused message=This payment is above your limit for a single payment.
    held: 202 status=under_review under_review=true message=under review sca=<none>
    ops without token: 401 error=not_operator
    review queue: 200 0.reference=tr-000012 0.severity=review 0.status=pending
    first approval: 200 status=awaiting_second_approval operator=op-1
    still held: 200 status=under_review message=under review
    same operator again: 409 error=same_operator
    approved: 200 status=approved operator=op-2
    released: 200 status=awaiting_sca under_review=false sca.factors=["totp","password"]
    held payment: 200 status=settled
    card: 201 status=active kind=virtual masked_pan=**** 0001 10/29 limits.online=true
    same card request: 201 id=card_000001
    freeze: 200 status=frozen
    purchase while frozen: 200 response_code=57 approved=false
    unfreeze: 200 status=active
    purchase: 200 response_code=00 approved=true
    display: 200 provider=fake card_id=card_000001 ui_secret=<none>
    consent: 200 factors=["otp"]
    pan token: 202 error=sca_required challenge.factors=["otp"]
    ("pan token" (status 200) (has_token true))
    limits, stale SCA: 403 error=sca_required
    step-up: 200 customer_id=cus-000001
    limits: 200 limits.daily.amount=200.00 limits.online=false
    standing order: 201 status=awaiting_sca next_run=2026-11-01 schedule=monthly:1
    standing orders: 200 0.status=active
    (runs (transfers 1))
    standing orders: 200 0.status=active 0.runs=1 0.next_run=2026-12-01
    statement: 200 opening.amount=0.00 closing.amount=14847.50
      1000.00 DEPOSIT 1000.00
      -100.00 SWAP-SELL 900.00
      -30.00 CAPTURE 870.00
      20000.00 DEPOSIT 20870.00
      -6000.00 CAPTURE 14870.00
      -12.50 CAPTURE 14857.50
      -10.00 CAPTURE 14847.50
    audit: 200 chain=ok
    ledger: 200 balanced=true trial_balance=[{"amount":"0.00000000","currency":"BTC"},{"amount":"0.00","currency":"EUR"}]
      out 10.00 sepa settled
      out 6000.00 sepa settled
      out 16000.00 sepa cancelled
      in 20000.00 sepa_instant settled
      out 30.00 sepa settled
      in 1000.00 sepa_instant settled
    |}];
  return ()
;;

let%expect_test "the same walk on PostgreSQL" =
  let%bind () =
    Pg.with_store (fun pg ->
      with_api
        ~store:(Neobank_api.Any_store.pack (module Neobank_api.Store_pg) pg)
        scenario)
  in
  [%expect
    {|
    me: 200 kyc.status=active kyc.tier=T2 accounts=6 sca_fresh=true
    eur: 200 iban=DE97500105170000000001 balance.available.amount=1000.00
    swap quote: 201 status=quoted sell.amount=100.00 buy.amount=0.00166666 price=0.0000166666
    swap execute: 200 status=awaiting_sca provider_sca.factors=["otp"]
    provider code: 200 status=accepted
    swap: 200 status=executed
    beneficiary: 201 destination=FR** **** 2606 rails=["sepa","sepa_instant"] trusted=false
    transfer: 201 status=awaiting_sca fee.amount=0.00 sca.factors=["totp","password"] sca.attempts_left=5
    same key, same body: 201 id=tr-000007
    ((same_body true) (replayed (true)))
    same key, other body: 422 error=idempotency_key_reused
    no key: 400 error=idempotency_key_required
    wrong password: 422 error=sca_failed message=the code or password is wrong; 4 attempts left challenge.attempts_left=4
    wrong code: 422 error=sca_failed message=the code or password is wrong; 3 attempts left challenge.attempts_left=3
    confirmed: 200 status=awaiting_sca provider_sca.factors=["otp"]
    provider code: 200 status=settled
    eur: 200 balance.available.amount=870.00
    (redelivered (deliveries 27) (all_ok true) (newly_applied 0))
    ("forged webhook" (status 401))
    after a day: 401
    above the cap: 422 error=payment_refused message=This payment is above your limit for a single payment.
    held: 202 status=under_review under_review=true message=under review sca=<none>
    ops without token: 401 error=not_operator
    review queue: 200 0.reference=tr-000012 0.severity=review 0.status=pending
    first approval: 200 status=awaiting_second_approval operator=op-1
    still held: 200 status=under_review message=under review
    same operator again: 409 error=same_operator
    approved: 200 status=approved operator=op-2
    released: 200 status=awaiting_sca under_review=false sca.factors=["totp","password"]
    held payment: 200 status=settled
    card: 201 status=active kind=virtual masked_pan=**** 0001 10/29 limits.online=true
    same card request: 201 id=card_000001
    freeze: 200 status=frozen
    purchase while frozen: 200 response_code=57 approved=false
    unfreeze: 200 status=active
    purchase: 200 response_code=00 approved=true
    display: 200 provider=fake card_id=card_000001 ui_secret=<none>
    consent: 200 factors=["otp"]
    pan token: 202 error=sca_required challenge.factors=["otp"]
    ("pan token" (status 200) (has_token true))
    limits, stale SCA: 403 error=sca_required
    step-up: 200 customer_id=cus-000001
    limits: 200 limits.daily.amount=200.00 limits.online=false
    standing order: 201 status=awaiting_sca next_run=2026-11-01 schedule=monthly:1
    standing orders: 200 0.status=active
    (runs (transfers 1))
    standing orders: 200 0.status=active 0.runs=1 0.next_run=2026-12-01
    statement: 200 opening.amount=0.00 closing.amount=14847.50
      1000.00 DEPOSIT 1000.00
      -100.00 SWAP-SELL 900.00
      -30.00 CAPTURE 870.00
      20000.00 DEPOSIT 20870.00
      -6000.00 CAPTURE 14870.00
      -12.50 CAPTURE 14857.50
      -10.00 CAPTURE 14847.50
    audit: 200 chain=ok
    ledger: 200 balanced=true trial_balance=[{"amount":"0.00000000","currency":"BTC"},{"amount":"0.00","currency":"EUR"}]
      out 10.00 sepa settled
      out 6000.00 sepa settled
      out 16000.00 sepa cancelled
      in 20000.00 sepa_instant settled
      out 30.00 sepa settled
      in 1000.00 sepa_instant settled
    |}];
  return ()
;;
