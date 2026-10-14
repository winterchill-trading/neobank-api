open! Core
open Async
open Neobank_api

let%expect_test "memory store" =
  let%bind () = Store_suite.run (module Store_memory) (Store_memory.create ()) in
  [%expect
    {|
    ("insert c1" (Ok Committed))
    ("same e-mail, other case" (Ok (Conflict email_taken)))
    ("same id" (Ok (Conflict customer_exists)))
    (lookups ((by_email (c1)) (by_ref (c1)) (missing false)))
    ("update c1" (Ok Committed))
    (c1
     (((id c1) (email customer@example.com) (password_hash <redacted>)
       (totp_secret <redacted>) (totp_confirmed true) (last_totp_step (7))
       (provider_ref (p1))
       (sca_lock
        ((consecutive_failures 0) (temporary_locks ()) (status Unlocked)))
       (failed_sca_at ()) (created_at "2026-10-14 09:00:00Z"))))
    ("e-mail cannot change" (Error "the e-mail address cannot change"))
    ("transfer with a conflicting customer" (Ok (Conflict customer_exists)))
    ("t-lost written" false)
    (session (Ok Committed))
    ("session found" true)
    (logout (Ok Committed))
    ("session after logout" false)
    ("kyc events" (Ok Committed))
    ("kyc event" (Ok Committed))
    (kyc (k1 k2 k3))
    (accounts (Ok Committed))
    (accounts (acc-btc acc-eur))
    (beneficiaries (Ok Committed))
    ("trust b1" (Ok Committed))
    (beneficiaries ((b1 true) (b2 false)))
    (transfers (Ok Committed))
    ("t1 cancelled" (Ok Committed))
    (transfers ((all ((t2 Draft) (t1 Cancelled))) (recent ((t2 Draft)))))
    (card (Ok Committed))
    ("card, same request" (Ok (Conflict card_request_exists)))
    (freeze (Ok Committed))
    (cards ((by_request (card-1)) (statuses (Frozen))))
    ("event e1" (Ok Committed))
    ("event e1 again" (Ok Duplicate_event))
    ("second batch written" false)
    ("event e1 with other content"
     (Error ("provider event id reused with different content" (event_id e1))))
    ("inbox has e1" true)
    (enqueue (Ok Committed))
    ("m1 sent" (Ok Committed))
    ("unknown message" (Error ("no such outbox message" nope)))
    (outbox ((due_0 (m1)) (due_5 (m1 m2)) (due_after (m2)) (sent ((m1 1)))))
    (idempotency
     (Started In_progress (Replay (status 201) (body "{\"id\":\"t1\"}")) Mismatch
      Started Started))
    (reviews (Ok Committed))
    (reviews ((pending (r1 r3)) (all (r1 r2 r3))))
    (audit (Ok Committed))
    (audit (Ok Committed))
    (audit
     ((actions ((1 login) (2 transfer.create) (3 transfer.confirm)))
      (chain (Ok ())) (tampered (Error ("audit chain broken" (seq 2))))))
    (project (Ok Committed))
    ("project again" (Ok Committed))
    (ledger
     ((head_before 0) (head 4)
      (postings
       ((3 1 DEPOSIT Credit "100.00 EUR" "2026-10-14 09:00:01Z")
        (4 1 DEPOSIT Credit "25.50 EUR" "2026-10-14 09:00:02Z")))
      (trial ("0.00 EUR"))))
    (pruned ((idempotency_keys 2) (inbox_rows 1) (sessions 0)))
    ("inbox has e1 after pruning" false)
    |}];
  return ()
;;

let%expect_test "postgres store: the same transcript" =
  let%bind () = Pg.with_store (fun store -> Store_suite.run (module Store_pg) store) in
  [%expect
    {|
    ("insert c1" (Ok Committed))
    ("same e-mail, other case" (Ok (Conflict email_taken)))
    ("same id" (Ok (Conflict customer_exists)))
    (lookups ((by_email (c1)) (by_ref (c1)) (missing false)))
    ("update c1" (Ok Committed))
    (c1
     (((id c1) (email customer@example.com) (password_hash <redacted>)
       (totp_secret <redacted>) (totp_confirmed true) (last_totp_step (7))
       (provider_ref (p1))
       (sca_lock
        ((consecutive_failures 0) (temporary_locks ()) (status Unlocked)))
       (failed_sca_at ()) (created_at "2026-10-14 09:00:00Z"))))
    ("e-mail cannot change" (Error "the e-mail address cannot change"))
    ("transfer with a conflicting customer" (Ok (Conflict customer_exists)))
    ("t-lost written" false)
    (session (Ok Committed))
    ("session found" true)
    (logout (Ok Committed))
    ("session after logout" false)
    ("kyc events" (Ok Committed))
    ("kyc event" (Ok Committed))
    (kyc (k1 k2 k3))
    (accounts (Ok Committed))
    (accounts (acc-btc acc-eur))
    (beneficiaries (Ok Committed))
    ("trust b1" (Ok Committed))
    (beneficiaries ((b1 true) (b2 false)))
    (transfers (Ok Committed))
    ("t1 cancelled" (Ok Committed))
    (transfers ((all ((t2 Draft) (t1 Cancelled))) (recent ((t2 Draft)))))
    (card (Ok Committed))
    ("card, same request" (Ok (Conflict card_request_exists)))
    (freeze (Ok Committed))
    (cards ((by_request (card-1)) (statuses (Frozen))))
    ("event e1" (Ok Committed))
    ("event e1 again" (Ok Duplicate_event))
    ("second batch written" false)
    ("event e1 with other content"
     (Error ("provider event id reused with different content" (event_id e1))))
    ("inbox has e1" true)
    (enqueue (Ok Committed))
    ("m1 sent" (Ok Committed))
    ("unknown message" (Error ("no such outbox message" nope)))
    (outbox ((due_0 (m1)) (due_5 (m1 m2)) (due_after (m2)) (sent ((m1 1)))))
    (idempotency
     (Started In_progress (Replay (status 201) (body "{\"id\":\"t1\"}")) Mismatch
      Started Started))
    (reviews (Ok Committed))
    (reviews ((pending (r1 r3)) (all (r1 r2 r3))))
    (audit (Ok Committed))
    (audit (Ok Committed))
    (audit
     ((actions ((1 login) (2 transfer.create) (3 transfer.confirm)))
      (chain (Ok ())) (tampered (Error ("audit chain broken" (seq 2))))))
    (project (Ok Committed))
    ("project again" (Ok Committed))
    (ledger
     ((head_before 0) (head 4)
      (postings
       ((3 1 DEPOSIT Credit "100.00 EUR" "2026-10-14 09:00:01Z")
        (4 1 DEPOSIT Credit "25.50 EUR" "2026-10-14 09:00:02Z")))
      (trial ("0.00 EUR"))))
    (pruned ((idempotency_keys 2) (inbox_rows 1) (sessions 0)))
    ("inbox has e1 after pruning" false)
    |}];
  return ()
;;

let%expect_test "postgres store: migrations apply once" =
  let%bind url = Pg.fresh_database_url () in
  let%bind first = Store_pg.connect ~url () >>| ok_exn in
  let%bind second = Store_pg.connect ~url () >>| ok_exn in
  print_s
    [%message
      ""
        ~first:(Store_pg.applied_migrations first : int list)
        ~second:(Store_pg.applied_migrations second : int list)];
  let%bind () = Store_pg.close first in
  let%bind () = Store_pg.close second in
  [%expect {| ((first (1 2)) (second ())) |}];
  return ()
;;
