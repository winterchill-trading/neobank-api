open! Core
open Async
open Pg_async
open Model
module Write = Store.Write
module Commit_result = Store.Commit_result

type t =
  { pool : Pool.t
  ; applied : int list
  }

let kind = "postgres"
let describe t = Config.to_string_hum (Pool.config t.pool)
let applied_migrations t = t.applied

let migrations =
  List.map Embedded_migrations.all ~f:(fun (version, name, sql) ->
    { Migrate.Migration.version; name; sql })
;;

let connect ?(max_size = 8) ?url () =
  let open Deferred.Or_error.Let_syntax in
  let%bind config =
    Deferred.return
      (match url with
       | Some url -> Config.of_url url
       | None -> Config.of_env ())
  in
  let%bind conn = Deferred.map (Connection.connect config) ~f:Pg_error.to_or_error in
  let%bind.Deferred applied = Migrate.apply conn migrations in
  let%bind.Deferred () = Connection.close conn in
  let%map applied =
    Deferred.return (Result.map_error applied ~f:Migrate.Error.to_error)
  in
  { pool = Pool.create ~max_size config; applied }
;;

let close t = Pool.close t.pool

(* Bin_prot bodies *)

let encode writer v = Bin_prot.Writer.to_string writer v

let decode reader s =
  Result.map_error
    (Result.try_with (fun () -> Bin_prot.Reader.of_string reader s))
    ~f:(fun exn -> Exn.to_string exn)
;;

let body_type reader =
  Type.map Type.bytea ~name:"body" ~f:(decode reader) ~f_inv:(fun _ -> "")
;;

(* Row decoders over the [body] column. *)
let body reader =
  let open Row.Let_syntax in
  let%map_open v = column "body" (body_type reader) in
  v
;;

let or_error d = Deferred.map d ~f:Pg_error.to_or_error

(* A query on the pool, as Or_error. *)
let fetch_all t sql ~params ~row = or_error (Pool.fetch_all t.pool sql ~params ~row)
let fetch_opt t sql ~params ~row = or_error (Pool.fetch_opt t.pool sql ~params ~row)
let utc = Param.time_ns
let cid c = Param.string (Customer_id.to_string c)

(* Writes, inside a transaction. [Error (`Stop result)] ends the batch with a domain
   result and rolls it back; [Error (`Pg e)] is a database failure. *)

type stop =
  [ `Stop of Commit_result.t Or_error.t
  | `Pg of Pg_error.t
  ]

let exec conn sql params : (int, stop) Result.t Deferred.t =
  Deferred.map (Query.execute conn sql ~params) ~f:(Result.map_error ~f:(fun e -> `Pg e))
;;

let unique_conflicts =
  [ "customers_pkey", "customer_exists"
  ; "customers_email_key", "email_taken"
  ; "cards_request_id_key", "card_request_exists"
  ]
;;

(* A unique violation on a known constraint is a domain conflict, not a failure. *)
let exec_unique conn sql params =
  match%map Query.execute conn sql ~params with
  | Ok n -> Ok n
  | Error (Server { sqlstate = "23505"; constraint_ = Some c; _ } as e) ->
    (match List.Assoc.find unique_conflicts c ~equal:String.equal with
     | Some name -> Error (`Stop (Ok (Commit_result.Conflict name)))
     | None -> Error (`Pg e))
  | Error e -> Error (`Pg e)
;;

let stop_error e : (_, stop) Result.t Deferred.t = return (Error (`Stop (Error e)))

let customer_params (c : Customer.t) =
  [ cid c.id
  ; Param.string c.email
  ; Param.option Type.string (Option.map c.provider_ref ~f:Customer_ref.to_string)
  ; utc c.created_at
  ; Param.bytea (encode Customer.Stable.V1.bin_writer_t c)
  ]
;;

let apply_write conn (w : Write.t) : (unit, stop) Result.t Deferred.t =
  let open Deferred.Result.Let_syntax in
  let ignore_count d = Deferred.Result.ignore_m d in
  match w with
  | Insert_customer c ->
    exec_unique
      conn
      "INSERT INTO customers (id, email, provider_ref, created_at, body) VALUES ($1, $2, \
       $3, $4, $5)"
      (customer_params c)
    |> ignore_count
  | Update_customer c ->
    let%bind n =
      exec
        conn
        "UPDATE customers SET provider_ref = $3, created_at = $4, body = $5 WHERE id = \
         $1 AND email = $2"
        (customer_params c)
    in
    if n = 1
    then return ()
    else (
      let%bind exists =
        Deferred.map
          (Query.fetch_opt
             conn
             "SELECT 1 AS one FROM customers WHERE id = $1"
             ~params:[ cid c.id ]
             ~row:Row.(column "one" Type.int))
          ~f:(Result.map_error ~f:(fun e -> `Pg e))
      in
      match exists with
      | None ->
        stop_error (Error.create_s [%message "no such customer" (c.id : Customer_id.t)])
      | Some _ -> stop_error (Error.of_string "the e-mail address cannot change"))
  | Put_session s ->
    exec
      conn
      "INSERT INTO sessions (token_hash, customer_id, last_activity, body) VALUES ($1, \
       $2, $3, $4) ON CONFLICT (token_hash) DO UPDATE SET last_activity = \
       EXCLUDED.last_activity, body = EXCLUDED.body"
      [ Param.string s.token_hash
      ; cid s.customer
      ; utc s.last_activity
      ; Param.bytea (encode Session.Stable.V1.bin_writer_t s)
      ]
    |> ignore_count
  | Delete_session { token_hash } ->
    exec conn "DELETE FROM sessions WHERE token_hash = $1" [ Param.string token_hash ]
    |> ignore_count
  | Kyc_event e ->
    exec
      conn
      "INSERT INTO kyc_events (customer_id, event_id, at, body) VALUES ($1, $2, $3, $4) \
       ON CONFLICT (customer_id, event_id) DO NOTHING"
      [ cid e.customer_id
      ; Param.string (Kyc_core.Event_id.to_string e.id)
      ; utc e.at
      ; Param.bytea (encode Kyc_core.Event.Stable.V1.bin_writer_t e)
      ]
    |> ignore_count
  | Put_account a ->
    exec
      conn
      "INSERT INTO accounts (id, customer_id, currency, updated_at, body) VALUES ($1, \
       $2, $3, $4, $5) ON CONFLICT (id) DO UPDATE SET customer_id = \
       EXCLUDED.customer_id, currency = EXCLUDED.currency, updated_at = \
       EXCLUDED.updated_at, body = EXCLUDED.body"
      [ Param.string (Account_ref.to_string (Account.id a))
      ; cid a.customer
      ; Param.string (Money.Currency.to_string (Account.currency a))
      ; utc a.updated_at
      ; Param.bytea (encode Account.Stable.V1.bin_writer_t a)
      ]
    |> ignore_count
  | Put_beneficiary b ->
    exec
      conn
      "INSERT INTO beneficiaries (id, customer_id, body) VALUES ($1, $2, $3) ON CONFLICT \
       (id) DO UPDATE SET customer_id = EXCLUDED.customer_id, body = EXCLUDED.body"
      [ Param.string (Payments_core.Beneficiary.Id.to_string b.id)
      ; Param.string b.owner
      ; Param.bytea (encode Payments_core.Beneficiary.Stable.V1.bin_writer_t b)
      ]
    |> ignore_count
  | Put_transfer x ->
    let under_review =
      match x.hold with
      | Under_review _ -> true
      | Not_held | Released _ | Refused _ -> false
    in
    exec
      conn
      "INSERT INTO transfers (id, customer_id, account_id, status, under_review, \
       created_at, updated_at, body) VALUES ($1, $2, $3, $4, $5, $6, $7, $8) ON CONFLICT \
       (id) DO UPDATE SET status = EXCLUDED.status, under_review = \
       EXCLUDED.under_review, updated_at = EXCLUDED.updated_at, body = EXCLUDED.body"
      [ Param.string (Payments_core.Transfer.Id.to_string (Transfer.id x))
      ; cid x.customer
      ; Param.string (Account_ref.to_string x.account)
      ; Param.string (Payments_core.Transfer.Status.name x.transfer.status)
      ; Param.bool under_review
      ; utc x.created_at
      ; utc x.updated_at
      ; Param.bytea (encode Transfer.Stable.V1.bin_writer_t x)
      ]
    |> ignore_count
  | Put_swap s ->
    exec
      conn
      "INSERT INTO swaps (id, customer_id, status, created_at, body) VALUES ($1, $2, $3, \
       $4, $5) ON CONFLICT (id) DO UPDATE SET status = EXCLUDED.status, body = \
       EXCLUDED.body"
      [ Param.string (Payments_core.Swap.Id.to_string s.id)
      ; cid s.customer
      ; Param.string (Sexp.to_string [%sexp (s.status : Swap_status.t)])
      ; utc s.created_at
      ; Param.bytea (encode Swap.Stable.V1.bin_writer_t s)
      ]
    |> ignore_count
  | Insert_card c | Put_card c ->
    let sql =
      match w with
      | Insert_card _ ->
        "INSERT INTO cards (id, customer_id, request_id, status, body) VALUES ($1, $2, \
         $3, $4, $5)"
      | _ ->
        "INSERT INTO cards (id, customer_id, request_id, status, body) VALUES ($1, $2, \
         $3, $4, $5) ON CONFLICT (id) DO UPDATE SET status = EXCLUDED.status, body = \
         EXCLUDED.body"
    in
    exec_unique
      conn
      sql
      [ Param.string (Card_core.Ids.Card_id.to_string c.id)
      ; cid c.customer
      ; Param.string c.request_id
      ; Param.string (Sexp.to_string [%sexp (c.status : Card_core.Card.Status.t)])
      ; Param.bytea (encode Card.Stable.V1.bin_writer_t c)
      ]
    |> ignore_count
  | Put_standing_order o ->
    exec
      conn
      "INSERT INTO standing_orders (id, customer_id, provider_ref, status, body) VALUES \
       ($1, $2, $3, $4, $5) ON CONFLICT (id) DO UPDATE SET provider_ref = \
       EXCLUDED.provider_ref, status = EXCLUDED.status, body = EXCLUDED.body"
      [ Param.string (Payments_core.Standing_order.Id.to_string (Standing_order.id o))
      ; cid o.customer
      ; Param.option
          Type.string
          (Option.map o.provider_ref ~f:Banking_provider.Standing_order_ref.to_string)
      ; Param.string
          (Sexp.to_string
             [%sexp (o.order.status : Payments_core.Standing_order.Status.t)])
      ; Param.bytea (encode Standing_order.Stable.V1.bin_writer_t o)
      ]
    |> ignore_count
  | Put_review r ->
    exec
      conn
      "INSERT INTO reviews (id, reference, customer_id, pending, submitted_at, body) \
       VALUES ($1, $2, $3, $4, $5, $6) ON CONFLICT (id) DO UPDATE SET pending = \
       EXCLUDED.pending, body = EXCLUDED.body"
      [ Param.string r.id
      ; Param.string r.reference
      ; cid r.customer
      ; Param.bool (Review.is_pending r)
      ; utc r.submitted_at
      ; Param.bytea (encode Review.Stable.V1.bin_writer_t r)
      ]
    |> ignore_count
  | Enqueue m | Update_outbox m ->
    let params =
      [ Param.string m.id
      ; Param.string (Outbox.Kind.aggregate_id m.kind)
      ; Param.string (Outbox.Kind.name m.kind)
      ; Param.int m.attempts
      ; utc m.next_attempt_at
      ; utc m.created_at
      ; Param.option
          Type.time_ns
          (match m.state with
           | Sent { at } -> Some at
           | Pending | Needs_review -> None)
      ; Param.string (Outbox.State.name m.state)
      ; Param.option Type.string m.last_error
      ; Param.bytea (encode Outbox.Stable.V1.bin_writer_t m)
      ]
    in
    (match w with
     | Enqueue _ ->
       exec
         conn
         "INSERT INTO outbox (id, aggregate_id, kind, attempts, next_attempt_at, \
          created_at, sent_at, state, last_error, payload) VALUES ($1, $2, $3, $4, $5, \
          $6, $7, $8, $9, $10)"
         params
       |> ignore_count
     | _ ->
       let%bind n =
         exec
           conn
           "UPDATE outbox SET aggregate_id = $2, kind = $3, attempts = $4, \
            next_attempt_at = $5, created_at = $6, sent_at = $7, state = $8, last_error \
            = $9, payload = $10 WHERE id = $1"
           params
       in
       if n = 1
       then return ()
       else stop_error (Error.create_s [%message "no such outbox message" m.id]))
  | Inbox e ->
    let id = Banking_provider.Event_id.to_string e.event_id in
    let%bind n =
      exec
        conn
        "INSERT INTO inbox (provider_event_id, received_at, payload_hash) VALUES ($1, \
         $2, $3) ON CONFLICT (provider_event_id) DO NOTHING"
        [ Param.string id; utc e.received_at; Param.string e.payload_hash ]
    in
    if n = 1
    then return ()
    else (
      let%bind old =
        Deferred.map
          (Query.fetch_one
             conn
             "SELECT payload_hash FROM inbox WHERE provider_event_id = $1"
             ~params:[ Param.string id ]
             ~row:Row.(column "payload_hash" Type.string))
          ~f:(Result.map_error ~f:(fun e -> `Pg e))
      in
      if String.equal old e.payload_hash
      then Deferred.return (Error (`Stop (Ok Commit_result.Duplicate_event)))
      else
        stop_error
          (Error.create_s
             [%message
               "provider event id reused with different content"
                 ~event_id:(e.event_id : Banking_provider.Event_id.t)]))
  | Provider_link { kind; key; value; at } ->
    exec
      conn
      "INSERT INTO provider_links (kind, key, value, updated_at) VALUES ($1, $2, $3, $4) \
       ON CONFLICT (kind, key) DO UPDATE SET value = EXCLUDED.value, updated_at = \
       EXCLUDED.updated_at"
      [ Param.string kind; Param.string key; Param.bytea value; utc at ]
    |> ignore_count
  | Ledger_record r ->
    let%bind n =
      exec
        conn
        "INSERT INTO ledger_records (seq, recorded_at, body) VALUES ($1, $2, $3) ON \
         CONFLICT (seq) DO NOTHING"
        [ Param.int r.seq
        ; utc (Ledger.Record.Event.recorded_at r.event)
        ; Param.bytea (encode Ledger.Record.Stable.V1.bin_writer_t r)
        ]
    in
    if n = 0
    then return ()
    else
      Deferred.List.fold (Ledger_posting.of_record r) ~init:(Ok ()) ~f:(fun acc p ->
        match acc with
        | Error _ -> Deferred.return acc
        | Ok () ->
          let signed = Ledger_posting.signed p in
          exec
            conn
            "INSERT INTO ledger_postings (seq, line, entry_key, account_id, side, \
             currency, amount_minor, code, reference, recorded_at) VALUES ($1, $2, $3, \
             $4, $5, $6, $7, $8, $9, $10)"
            [ Param.int p.seq
            ; Param.int p.line
            ; Param.string p.entry_key
            ; Param.string p.account
            ; Param.string
                (match p.side with
                 | Debit -> "debit"
                 | Credit -> "credit")
            ; Param.string (Money.Currency.to_string (Money.Amount.currency signed))
            ; Param.numeric (Z.to_string (Money.Amount.minor_units signed))
            ; Param.string p.code
            ; Param.string p.reference
            ; utc p.recorded_at
            ]
          |> ignore_count)
  | Audit record ->
    (* One writer at a time extends the chain: the transaction-scoped advisory lock (key:
       "audit" in ASCII) serializes appends, so two never share a predecessor. *)
    let%bind (_ : int) = exec conn "SELECT pg_advisory_xact_lock(1635083369)" [] in
    let%bind last =
      Deferred.map
        (Query.fetch_opt
           conn
           "SELECT seq, hash FROM audit ORDER BY seq DESC LIMIT 1"
           ~params:[]
           ~row:
             (let open Row.Let_syntax in
              let%map_open seq = column "seq" int
              and hash = column "hash" string in
              seq, hash))
        ~f:(Result.map_error ~f:(fun e -> `Pg e))
    in
    let prev_hash, seq =
      match last with
      | None -> Audit.genesis_hash, 1
      | Some (seq, hash) -> hash, seq + 1
    in
    exec
      conn
      "INSERT INTO audit (seq, at, actor, on_behalf_of, action, object_id, source, \
       outcome, prev_hash, hash, body) VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, \
       $11)"
      [ Param.int seq
      ; utc record.at
      ; Param.string record.actor
      ; Param.option Type.string record.on_behalf_of
      ; Param.string record.action
      ; Param.string record.object_id
      ; Param.string record.source
      ; Param.string record.outcome
      ; Param.string prev_hash
      ; Param.string (Audit.chain_hash ~prev_hash record)
      ; Param.bytea (encode Audit.Record.Stable.V1.bin_writer_t record)
      ]
    |> ignore_count
;;

let commit t writes =
  let%map result =
    Pool.with_transaction
      t.pool
      ~isolation:Serializable
      ~commit_if:(function
        | Ok Commit_result.Committed -> true
        | Ok (Duplicate_event | Conflict _) | Error _ -> false)
      ~f:(fun conn ->
        match%map
          Deferred.List.fold writes ~init:(Ok ()) ~f:(fun acc w ->
            match acc with
            | Error _ -> return acc
            | Ok () -> apply_write conn w)
        with
        | Ok () -> Ok (Ok Commit_result.Committed)
        | Error (`Stop result) -> Ok result
        | Error (`Pg e) -> Error e)
  in
  Or_error.join (Pg_error.to_or_error result)
;;

(* Reads *)

let customer_row = body Customer.Stable.V1.bin_reader_t

let customer t id =
  fetch_opt
    t
    "SELECT body FROM customers WHERE id = $1"
    ~params:[ cid id ]
    ~row:customer_row
;;

let customer_by_email t email =
  fetch_opt
    t
    "SELECT body FROM customers WHERE email = $1"
    ~params:[ Param.string (Customer.normalize_email email) ]
    ~row:customer_row
;;

let customer_by_provider_ref t r =
  fetch_opt
    t
    "SELECT body FROM customers WHERE provider_ref = $1"
    ~params:[ Param.string (Customer_ref.to_string r) ]
    ~row:customer_row
;;

let session t ~token_hash =
  fetch_opt
    t
    "SELECT body FROM sessions WHERE token_hash = $1"
    ~params:[ Param.string token_hash ]
    ~row:(body Session.Stable.V1.bin_reader_t)
;;

let kyc_events t id =
  fetch_all
    t
    "SELECT body FROM kyc_events WHERE customer_id = $1 ORDER BY seq"
    ~params:[ cid id ]
    ~row:(body Kyc_core.Event.Stable.V1.bin_reader_t)
;;

let account_row = body Account.Stable.V1.bin_reader_t

let account t id =
  fetch_opt
    t
    "SELECT body FROM accounts WHERE id = $1"
    ~params:[ Param.string (Account_ref.to_string id) ]
    ~row:account_row
;;

let accounts t id =
  fetch_all
    t
    "SELECT body FROM accounts WHERE customer_id = $1 ORDER BY currency COLLATE \"C\", \
     id COLLATE \"C\""
    ~params:[ cid id ]
    ~row:account_row
;;

let beneficiary_row = body Payments_core.Beneficiary.Stable.V1.bin_reader_t

let beneficiary t id =
  fetch_opt
    t
    "SELECT body FROM beneficiaries WHERE id = $1"
    ~params:[ Param.string (Payments_core.Beneficiary.Id.to_string id) ]
    ~row:beneficiary_row
;;

let beneficiaries t id =
  fetch_all
    t
    "SELECT body FROM beneficiaries WHERE customer_id = $1 ORDER BY seq"
    ~params:[ cid id ]
    ~row:beneficiary_row
;;

let transfer_row = body Transfer.Stable.V1.bin_reader_t

let transfer t id =
  fetch_opt
    t
    "SELECT body FROM transfers WHERE id = $1"
    ~params:[ Param.string (Payments_core.Transfer.Id.to_string id) ]
    ~row:transfer_row
;;

let transfers t id ?since () =
  match since with
  | None ->
    fetch_all
      t
      "SELECT body FROM transfers WHERE customer_id = $1 ORDER BY seq DESC"
      ~params:[ cid id ]
      ~row:transfer_row
  | Some since ->
    fetch_all
      t
      "SELECT body FROM transfers WHERE customer_id = $1 AND created_at >= $2 ORDER BY \
       seq DESC"
      ~params:[ cid id; utc since ]
      ~row:transfer_row
;;

let swap_row = body Swap.Stable.V1.bin_reader_t

let swap t id =
  fetch_opt
    t
    "SELECT body FROM swaps WHERE id = $1"
    ~params:[ Param.string (Payments_core.Swap.Id.to_string id) ]
    ~row:swap_row
;;

let swaps t id =
  fetch_all
    t
    "SELECT body FROM swaps WHERE customer_id = $1 ORDER BY seq DESC"
    ~params:[ cid id ]
    ~row:swap_row
;;

let card_row = body Card.Stable.V1.bin_reader_t

let card t id =
  fetch_opt
    t
    "SELECT body FROM cards WHERE id = $1"
    ~params:[ Param.string (Card_core.Ids.Card_id.to_string id) ]
    ~row:card_row
;;

let card_by_request t ~request_id =
  fetch_opt
    t
    "SELECT body FROM cards WHERE request_id = $1"
    ~params:[ Param.string request_id ]
    ~row:card_row
;;

let cards t id =
  fetch_all
    t
    "SELECT body FROM cards WHERE customer_id = $1 ORDER BY seq"
    ~params:[ cid id ]
    ~row:card_row
;;

let standing_order_row = body Standing_order.Stable.V1.bin_reader_t

let standing_order t id =
  fetch_opt
    t
    "SELECT body FROM standing_orders WHERE id = $1"
    ~params:[ Param.string (Payments_core.Standing_order.Id.to_string id) ]
    ~row:standing_order_row
;;

let standing_order_by_provider_ref t r =
  fetch_opt
    t
    "SELECT body FROM standing_orders WHERE provider_ref = $1 ORDER BY seq LIMIT 1"
    ~params:[ Param.string (Banking_provider.Standing_order_ref.to_string r) ]
    ~row:standing_order_row
;;

let standing_orders t id =
  fetch_all
    t
    "SELECT body FROM standing_orders WHERE customer_id = $1 ORDER BY seq"
    ~params:[ cid id ]
    ~row:standing_order_row
;;

let review_row = body Review.Stable.V1.bin_reader_t

let review t id =
  fetch_opt
    t
    "SELECT body FROM reviews WHERE id = $1"
    ~params:[ Param.string id ]
    ~row:review_row
;;

let reviews t ~pending_only =
  if pending_only
  then
    fetch_all
      t
      "SELECT body FROM reviews WHERE pending ORDER BY seq"
      ~params:[]
      ~row:review_row
  else fetch_all t "SELECT body FROM reviews ORDER BY seq" ~params:[] ~row:review_row
;;

let outbox_row =
  let open Row.Let_syntax in
  let%map_open v = column "payload" (body_type Outbox.Stable.V1.bin_reader_t) in
  v
;;

let outbox_due t ~now ~limit =
  fetch_all
    t
    "SELECT payload FROM outbox WHERE state = 'pending' AND next_attempt_at <= $1 ORDER \
     BY seq LIMIT $2"
    ~params:[ utc now; Param.int limit ]
    ~row:outbox_row
;;

let outbox t id =
  fetch_opt
    t
    "SELECT payload FROM outbox WHERE id = $1"
    ~params:[ Param.string id ]
    ~row:outbox_row
;;

let outbox_in_state t ~state =
  fetch_all
    t
    "SELECT payload FROM outbox WHERE state = $1 ORDER BY seq"
    ~params:[ Param.string state ]
    ~row:outbox_row
;;

let inbox_mem t id =
  let%map.Deferred.Or_error row =
    fetch_opt
      t
      "SELECT 1 AS one FROM inbox WHERE provider_event_id = $1"
      ~params:[ Param.string (Banking_provider.Event_id.to_string id) ]
      ~row:Row.(column "one" Type.int)
  in
  Option.is_some row
;;

let scope_params (s : Idempotency.Scope.t) =
  [ Param.string (Idempotency.Scope.to_string s)
  ; Param.string s.customer
  ; Param.string s.meth
  ; Param.string s.route
  ; Param.string s.key
  ]
;;

let idempotency_begin t scope ~request_hash ~now =
  let open Deferred.Or_error.Let_syntax in
  let%bind inserted =
    or_error
      (Pool.execute
         t.pool
         "INSERT INTO idempotency (scope, customer_id, method, route, key, request_hash, \
          state, created_at) VALUES ($1, $2, $3, $4, $5, $6, 'in_progress', $7) ON \
          CONFLICT (scope) DO NOTHING"
         ~params:(scope_params scope @ [ Param.string request_hash; utc now ]))
  in
  if inserted = 1
  then return Idempotency.Begin.Started
  else (
    let%map hash, state, status, body =
      or_error
        (Pool.fetch_one
           t.pool
           "SELECT request_hash, state, status, body FROM idempotency WHERE scope = $1"
           ~params:[ Param.string (Idempotency.Scope.to_string scope) ]
           ~row:
             (let open Row.Let_syntax in
              let%map_open hash = column "request_hash" string
              and state = column "state" string
              and status = column "status" (nullable int)
              and body = column "body" (nullable string) in
              hash, state, status, body))
    in
    if not (String.equal hash request_hash)
    then Idempotency.Begin.Mismatch
    else (
      match state, status, body with
      | "completed", Some status, Some body -> Replay { status; body }
      | _ -> In_progress))
;;

let idempotency_complete t scope ~status ~body =
  let open Deferred.Or_error.Let_syntax in
  let%bind n =
    or_error
      (Pool.execute
         t.pool
         "UPDATE idempotency SET state = 'completed', status = $2, body = $3 WHERE scope \
          = $1"
         ~params:
           [ Param.string (Idempotency.Scope.to_string scope)
           ; Param.int status
           ; Param.string body
           ])
  in
  if n = 1
  then return ()
  else
    Deferred.Or_error.error_s
      [%message
        "no request under this idempotency key" ~key:(Idempotency.Scope.to_string scope)]
;;

let idempotency_abandon t scope =
  Deferred.Or_error.ignore_m
    (or_error
       (Pool.execute
          t.pool
          "DELETE FROM idempotency WHERE scope = $1"
          ~params:[ Param.string (Idempotency.Scope.to_string scope) ]))
;;

let provider_link t ~kind ~key =
  fetch_opt
    t
    "SELECT value FROM provider_links WHERE kind = $1 AND key = $2"
    ~params:[ Param.string kind; Param.string key ]
    ~row:Row.(column "value" Type.bytea)
;;

let ledger_head t =
  let%map.Deferred.Or_error head =
    fetch_opt
      t
      "SELECT max(seq) AS head FROM ledger_records"
      ~params:[]
      ~row:Row.(column "head" (Type.nullable Type.int))
  in
  Option.value (Option.join head) ~default:0
;;

let amount_of ~currency ~minor =
  let open Result.Let_syntax in
  let%bind currency =
    Money.Currency.of_string_or_error currency |> Result.map_error ~f:Error.to_string_hum
  in
  let%map minor =
    Result.try_with (fun () -> Z.of_string minor) |> Result.map_error ~f:Exn.to_string
  in
  Money.Amount.of_minor_units currency minor
;;

let ledger_postings t ~account =
  fetch_all
    t
    "SELECT seq, line, entry_key, account_id, side, currency, amount_minor::text AS \
     amount_minor, code, reference, recorded_at FROM ledger_postings WHERE account_id = \
     $1 ORDER BY seq, line"
    ~params:[ Param.string account ]
    ~row:
      (let open Row.Let_syntax in
       let%map_open seq = column "seq" int
       and line = column "line" int
       and entry_key = column "entry_key" string
       and account = column "account_id" string
       and side = column "side" string
       and currency = column "currency" string
       and minor = column "amount_minor" string
       and code = column "code" string
       and reference = column "reference" string
       and recorded_at = column "recorded_at" time_ns in
       let amount =
         amount_of ~currency ~minor |> Result.ok_or_failwith |> Money.Amount.abs
       in
       { Ledger_posting.seq
       ; line
       ; entry_key
       ; account
       ; side = (if String.equal side "debit" then Debit else Credit)
       ; amount
       ; code
       ; reference
       ; recorded_at
       })
;;

let ledger_trial_balance t =
  let%bind.Deferred.Or_error rows =
    fetch_all
      t
      "SELECT currency, sum(amount_minor)::text AS total FROM ledger_postings GROUP BY \
       currency ORDER BY currency COLLATE \"C\""
      ~params:[]
      ~row:
        (let open Row.Let_syntax in
         let%map_open currency = column "currency" string
         and total = column "total" string in
         currency, total)
  in
  Deferred.return
    (List.map rows ~f:(fun (currency, minor) ->
       amount_of ~currency ~minor |> Result.map_error ~f:Error.of_string)
     |> Result.all)
;;

let audit_log t =
  fetch_all
    t
    "SELECT seq, prev_hash, hash, body FROM audit ORDER BY seq"
    ~params:[]
    ~row:
      (let open Row.Let_syntax in
       let%map_open seq = column "seq" int
       and prev_hash = column "prev_hash" string
       and hash = column "hash" string
       and record = column "body" (body_type Audit.Record.Stable.V1.bin_reader_t) in
       { Audit.seq; record; prev_hash; hash })
;;

let prune t ~idempotency_before ~inbox_before ~sessions_before =
  let open Deferred.Or_error.Let_syntax in
  let delete sql before = or_error (Pool.execute t.pool sql ~params:[ utc before ]) in
  let%bind idempotency_keys =
    delete "DELETE FROM idempotency WHERE created_at < $1" idempotency_before
  in
  let%bind inbox_rows = delete "DELETE FROM inbox WHERE received_at < $1" inbox_before in
  let%map sessions =
    delete "DELETE FROM sessions WHERE last_activity < $1" sessions_before
  in
  { Store.Pruned.idempotency_keys; inbox_rows; sessions }
;;
