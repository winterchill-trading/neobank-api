(* One transcript of store behaviour; test_store.ml checks that every implementation
   prints the same one. *)

open! Core
open Async
open Neobank_api
open Fixtures
module Commit_result = Store.Commit_result

let run (type a) (module S : Store.S with type t = a) (store : a) =
  let ok d = Deferred.map d ~f:ok_exn in
  let commit label writes =
    let%map result = S.commit store writes in
    print_s [%message label ~_:(result : Commit_result.t Or_error.t)]
  in
  let show label v = print_s [%message label ~_:(v : Sexp.t)] in
  (* Customers *)
  let%bind () =
    commit "insert c1" [ Insert_customer (customer ~provider_ref:"p1" "c1") ]
  in
  let%bind () =
    commit
      "same e-mail, other case"
      [ Insert_customer (customer ~email:" Customer@Example.com" "c2") ]
  in
  let%bind () =
    commit "same id" [ Insert_customer (customer ~email:"other@example.com" "c1") ]
  in
  let%bind by_email = ok (S.customer_by_email store "CUSTOMER@example.com ") in
  let%bind by_ref =
    ok (S.customer_by_provider_ref store (Banking_provider.Customer_ref.of_string "p1"))
  in
  let%bind missing = ok (S.customer store (customer_id "nobody")) in
  show
    "lookups"
    [%sexp
      { by_email =
          (Option.map by_email ~f:(fun c -> Customer_id.to_string c.id) : string option)
      ; by_ref =
          (Option.map by_ref ~f:(fun c -> Customer_id.to_string c.id) : string option)
      ; missing = (Option.is_some missing : bool)
      }];
  let%bind () =
    commit
      "update c1"
      [ Update_customer
          { (customer ~provider_ref:"p1" "c1") with
            totp_confirmed = true
          ; last_totp_step = Some 7
          }
      ]
  in
  let%bind c1 = ok (S.customer store (customer_id "c1")) in
  show "c1" [%sexp (c1 : Model.Customer.t option)];
  let%bind () =
    commit
      "e-mail cannot change"
      [ Update_customer (customer ~email:"new@example.com" "c1") ]
  in
  (* A batch with a conflict writes nothing. *)
  let%bind () =
    commit
      "transfer with a conflicting customer"
      [ Put_transfer (transfer ~customer:"c1" ~account:"acc-eur" "t-lost")
      ; Insert_customer (customer "c1")
      ]
  in
  let%bind lost = ok (S.transfer store (Payments_core.Transfer.Id.of_string "t-lost")) in
  show "t-lost written" [%sexp (Option.is_some lost : bool)];
  (* Sessions *)
  let session : Model.Session.t =
    { token_hash = Model.Session.hash_token "token-1"
    ; customer = customer_id "c1"
    ; created_at = start
    ; last_activity = start
    ; sca_at = start
    }
  in
  let%bind () = commit "session" [ Put_session session ] in
  let%bind s = ok (S.session store ~token_hash:session.token_hash) in
  show "session found" [%sexp (Option.is_some s : bool)];
  let%bind () = commit "logout" [ Delete_session { token_hash = session.token_hash } ] in
  let%bind s = ok (S.session store ~token_hash:session.token_hash) in
  show "session after logout" [%sexp (Option.is_some s : bool)];
  (* KYC events keep their order. *)
  let kyc id kind : Kyc_core.Event.t =
    { id = Kyc_core.Event_id.of_string id
    ; customer_id = customer_id "c1"
    ; at = start
    ; actor = System
    ; note = None
    ; kind
    }
  in
  let%bind () =
    commit
      "kyc events"
      [ Kyc_event (kyc "k1" (Opened person))
      ; Kyc_event (kyc "k2" (Step_changed { step = Email; state = Sent }))
      ]
  in
  let%bind () = commit "kyc event" [ Kyc_event (kyc "k3" (Status_changed Submit)) ] in
  let%bind events = ok (S.kyc_events store (customer_id "c1")) in
  show
    "kyc"
    [%sexp (List.map events ~f:(fun e -> Kyc_core.Event_id.to_string e.id) : string list)];
  (* Accounts, ordered by currency. *)
  let%bind () =
    commit
      "accounts"
      [ Put_account (account ~customer:"c1" "acc-eur")
      ; Put_account (account ~currency:Money.Currency.btc ~customer:"c1" "acc-btc")
      ; Put_account (account ~customer:"c2" "acc-other")
      ]
  in
  let%bind accounts = ok (S.accounts store (customer_id "c1")) in
  show
    "accounts"
    [%sexp
      (List.map accounts ~f:(fun a ->
         Banking_provider.Account_ref.to_string (Model.Account.id a))
       : string list)];
  (* Beneficiaries *)
  let%bind () =
    commit
      "beneficiaries"
      [ Put_beneficiary (beneficiary ~owner:"c1" "b1")
      ; Put_beneficiary (beneficiary ~owner:"c1" "b2")
      ; Put_beneficiary (beneficiary ~owner:"c2" "b3")
      ]
  in
  let%bind () =
    commit
      "trust b1"
      [ Put_beneficiary (Payments_core.Beneficiary.trust (beneficiary ~owner:"c1" "b1")) ]
  in
  let%bind bens = ok (S.beneficiaries store (customer_id "c1")) in
  show
    "beneficiaries"
    [%sexp
      (List.map bens ~f:(fun b -> Payments_core.Beneficiary.Id.to_string b.id, b.trusted)
       : (string * bool) list)];
  (* Transfers: newest first; an update keeps the original age. *)
  let%bind () =
    commit
      "transfers"
      [ Put_transfer (transfer ~customer:"c1" ~account:"acc-eur" "t1")
      ; Put_transfer
          (transfer ~created_at:(at_min 5) ~customer:"c1" ~account:"acc-eur" "t2")
      ; Put_transfer (transfer ~customer:"c2" ~account:"acc-other" "t3")
      ]
  in
  let t1 = transfer ~customer:"c1" ~account:"acc-eur" "t1" in
  let%bind () =
    commit
      "t1 cancelled"
      [ Put_transfer
          { t1 with
            transfer =
              Payments_core.Transfer.transition t1.transfer ~to_:Cancelled ~at:(at_min 1)
              |> ok_exn
          }
      ]
  in
  let%bind all = ok (S.transfers store (customer_id "c1") ()) in
  let%bind recent = ok (S.transfers store (customer_id "c1") ~since:(at_min 1) ()) in
  let summary (l : Model.Transfer.t list) =
    List.map l ~f:(fun x ->
      ( Payments_core.Transfer.Id.to_string (Model.Transfer.id x)
      , Payments_core.Transfer.Status.name x.transfer.status ))
  in
  show
    "transfers"
    [%sexp
      { all = (summary all : (string * string) list)
      ; recent = (summary recent : (string * string) list)
      }];
  (* Cards: a request id is used once. *)
  let%bind () =
    commit "card" [ Insert_card (card ~customer:"c1" ~request_id:"r1" "card-1") ]
  in
  let%bind () =
    commit
      "card, same request"
      [ Insert_card (card ~customer:"c1" ~request_id:"r1" "card-2") ]
  in
  let%bind () =
    commit
      "freeze"
      [ Put_card { (card ~customer:"c1" ~request_id:"r1" "card-1") with status = Frozen }
      ]
  in
  let%bind by_request = ok (S.card_by_request store ~request_id:"r1") in
  let%bind cards = ok (S.cards store (customer_id "c1")) in
  show
    "cards"
    [%sexp
      { by_request =
          (Option.map by_request ~f:(fun c -> Card_core.Ids.Card_id.to_string c.id)
           : string option)
      ; statuses =
          (List.map cards ~f:(fun c -> c.status) : Card_core.Card.Status.Stable.V1.t list)
      }];
  (* Inbox: an event id applies once; the whole batch is skipped. *)
  let%bind () =
    commit
      "event e1"
      [ Inbox (Model.Inbox_entry.of_event (event "e1") ~received_at:start)
      ; Put_transfer (transfer ~customer:"c1" ~account:"acc-eur" "t-e1")
      ]
  in
  let%bind () =
    commit
      "event e1 again"
      [ Inbox (Model.Inbox_entry.of_event (event "e1") ~received_at:(at_min 1))
      ; Put_transfer (transfer ~customer:"c1" ~account:"acc-eur" "t-e1-again")
      ]
  in
  let%bind again =
    ok (S.transfer store (Payments_core.Transfer.Id.of_string "t-e1-again"))
  in
  show "second batch written" [%sexp (Option.is_some again : bool)];
  let%bind () =
    commit
      "event e1 with other content"
      [ Inbox
          (Model.Inbox_entry.of_event
             { (event "e1") with at = at_min 9 }
             ~received_at:(at_min 1))
      ]
  in
  let%bind mem = ok (S.inbox_mem store (Banking_provider.Event_id.of_string "e1")) in
  show "inbox has e1" [%sexp (mem : bool)];
  (* Outbox *)
  let message id ~now kind = Model.Outbox.create ~id ~now kind in
  let m1 = message "m1" ~now:start (Open_accounts (customer_id "c1")) in
  let m2 = message "m2" ~now:(at_min 2) (Create_customer (customer_id "c2")) in
  let%bind () = commit "enqueue" [ Enqueue m1; Enqueue m2 ] in
  let due now =
    let%map l = ok (S.outbox_due store ~now ~limit:10) in
    List.map l ~f:(fun (m : Model.Outbox.t) -> m.id)
  in
  let%bind due_0 = due start in
  let%bind due_5 = due (at_min 5) in
  let%bind () =
    commit
      "m1 sent"
      [ Update_outbox { m1 with state = Sent { at = at_min 1 }; attempts = 1 } ]
  in
  let%bind due_after = due (at_min 5) in
  let%bind sent = ok (S.outbox_in_state store ~state:"sent") in
  let%bind () =
    commit
      "unknown message"
      [ Update_outbox (message "nope" ~now:start (Open_accounts (customer_id "c1"))) ]
  in
  show
    "outbox"
    [%sexp
      { due_0 : string list
      ; due_5 : string list
      ; due_after : string list
      ; sent =
          (List.map sent ~f:(fun (m : Model.Outbox.t) -> m.id, m.attempts)
           : (string * int) list)
      }];
  (* Idempotency keys *)
  let scope : Model.Idempotency.Scope.t =
    { customer = "c1"; meth = "POST"; route = "/transfers"; key = "k-1" }
  in
  let begin_ hash now = ok (S.idempotency_begin store scope ~request_hash:hash ~now) in
  let%bind first = begin_ "h1" start in
  let%bind while_running = begin_ "h1" start in
  let%bind () =
    ok (S.idempotency_complete store scope ~status:201 ~body:"{\"id\":\"t1\"}")
  in
  let%bind replay = begin_ "h1" (at_min 1) in
  let%bind other_body = begin_ "h2" (at_min 1) in
  let%bind other_route =
    ok
      (S.idempotency_begin
         store
         { scope with route = "/swaps" }
         ~request_hash:"h2"
         ~now:start)
  in
  let%bind () = ok (S.idempotency_abandon store { scope with route = "/swaps" }) in
  let%bind after_abandon =
    ok
      (S.idempotency_begin
         store
         { scope with route = "/swaps" }
         ~request_hash:"h3"
         ~now:start)
  in
  show
    "idempotency"
    [%sexp
      ([ first; while_running; replay; other_body; other_route; after_abandon ]
       : Model.Idempotency.Begin.t list)];
  (* Reviews *)
  let review id status : Model.Review.t =
    { id
    ; reference = "t1"
    ; customer = customer_id "c1"
    ; risk =
        { context =
            { customer = "c1"
            ; tier = Risk_rules.Tier.of_string "basic"
            ; now = start
            ; balances = []
            ; subject =
                Transfer
                  (Risk_rules.Context.Transfer.create (draft ~owner:"c1" "t1") |> ok_exn)
            ; history = []
            ; incoming = []
            ; card_history = []
            ; geo = Risk_rules.Context.Geo.unknown
            ; session = Risk_rules.Context.Session.unknown
            }
        ; decision = Risk_rules.Decision.allow
        }
    ; submitted_at = start
    ; status
    ; notes = []
    }
  in
  let%bind () =
    commit
      "reviews"
      [ Put_review (review "r1" Pending)
      ; Put_review (review "r2" (Approved { operator = "op"; at = at_min 1 }))
      ; Put_review (review "r3" Pending)
      ]
  in
  let%bind pending = ok (S.reviews store ~pending_only:true) in
  let%bind all = ok (S.reviews store ~pending_only:false) in
  show
    "reviews"
    [%sexp
      { pending = (List.map pending ~f:(fun r -> r.id) : string list)
      ; all = (List.map all ~f:(fun r -> r.id) : string list)
      }];
  (* Audit chain *)
  let%bind () =
    commit "audit" [ Audit (audit "login"); Audit (audit "transfer.create") ]
  in
  let%bind () = commit "audit" [ Audit (audit "transfer.confirm") ] in
  let%bind log = ok (S.audit_log store) in
  show
    "audit"
    [%sexp
      { actions =
          (List.map log ~f:(fun a -> a.seq, a.record.action) : (int * string) list)
      ; chain = (Model.Audit.verify_chain log : unit Or_error.t)
      ; tampered =
          (Model.Audit.verify_chain
             (List.map log ~f:(fun a ->
                if a.seq = 2
                then { a with record = { a.record with outcome = "edited" } }
                else a))
           : unit Or_error.t)
      }];
  (* Ledger projection: records from a real journal, projected once. *)
  let records =
    let dir = Filename_unix.temp_dir "neobank-store" "ledger" in
    let clock = Async_kernel.Time_source.create ~now:start () in
    let journal =
      Ledger.Journal.create
        ~time_source:(Async_kernel.Time_source.read_only clock)
        ~fsync:false
        (dir ^/ "journal")
      |> ok_exn
    in
    let account id kind ?allow_negative () =
      Ledger.Journal.open_account
        journal
        (Ledger.Account.create ?allow_negative (Ledger.Account_id.of_string id) kind)
      |> ok_exn
      |> ignore
    in
    account "cash" Asset ~allow_negative:true ();
    account "customer:c1/eur" Liability ();
    let book key amount =
      Async_kernel.Time_source.advance_directly
        clock
        ~to_:(Time_ns.add (Async_kernel.Time_source.now clock) (Time_ns.Span.of_sec 1.));
      let a = eur amount in
      Ledger.Journal.transfer
        journal
        ~idempotency_key:(Ledger.Idempotency_key.of_string key)
        ~code:Deposit
        ~reference:key
        ~effective_at:(Async_kernel.Time_source.now clock)
        [ Ledger.Posting.debit (Ledger.Account_id.of_string "cash") a
        ; Ledger.Posting.credit (Ledger.Account_id.of_string "customer:c1/eur") a
        ]
      |> ok_exn
      |> ignore
    in
    book "dep-1" "100.00";
    book "dep-2" "25.50";
    let records = Ledger.Journal.records journal in
    Ledger.Journal.close journal;
    Core_unix.system (sprintf "rm -rf %s" (Filename.quote dir)) |> ignore;
    records
  in
  let%bind head_before = ok (S.ledger_head store) in
  let%bind () =
    commit "project" (List.map records ~f:(fun r -> Store.Write.Ledger_record r))
  in
  let%bind () =
    commit "project again" (List.map records ~f:(fun r -> Store.Write.Ledger_record r))
  in
  let%bind head = ok (S.ledger_head store) in
  let%bind postings = ok (S.ledger_postings store ~account:"customer:c1/eur") in
  let%bind trial = ok (S.ledger_trial_balance store) in
  show
    "ledger"
    [%sexp
      { head_before : int
      ; head : int
      ; postings =
          (List.map postings ~f:(fun p ->
             p.seq, p.line, p.code, p.side, Money.Amount.to_string p.amount, p.recorded_at)
           : (int * int * string * Ledger.Side.t * string * Payments_core.Utc_time.t) list)
      ; trial = (List.map trial ~f:Money.Amount.to_string : string list)
      }];
  (* Pruning *)
  let%bind pruned =
    ok
      (S.prune
         store
         ~idempotency_before:(at_min 1)
         ~inbox_before:(at_min 1)
         ~sessions_before:(at_min 1))
  in
  show "pruned" [%sexp (pruned : Store.Pruned.t)];
  let%bind mem = ok (S.inbox_mem store (Banking_provider.Event_id.of_string "e1")) in
  show "inbox has e1 after pruning" [%sexp (mem : bool)];
  return ()
;;
