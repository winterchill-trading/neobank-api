open! Core
open Async_kernel
open Model
module Write = Store.Write
module Commit_result = Store.Commit_result

(* A value with the sequence number of its first insertion, so listings can be ordered by
   age without trusting clocks. *)
type 'a aged =
  { seq : int
  ; value : 'a
  }

type idem =
  { request_hash : string
  ; mutable completed : (int * string) option
  ; created_at : Time_ns.t
  }

type t =
  { customers : Customer.t String.Table.t
  ; emails : string String.Table.t
  ; provider_refs : string String.Table.t
  ; sessions : Session.t String.Table.t
  ; kyc : Kyc_core.Event.t list String.Table.t (* newest first *)
  ; accounts : Account.t String.Table.t
  ; beneficiaries : Payments_core.Beneficiary.t aged String.Table.t
  ; transfers : Transfer.t aged String.Table.t
  ; swaps : Swap.t aged String.Table.t
  ; cards : Card.t aged String.Table.t
  ; card_requests : string String.Table.t
  ; standing_orders : Standing_order.t aged String.Table.t
  ; reviews : Review.t aged String.Table.t
  ; outbox : Outbox.t aged String.Table.t
  ; inbox : Inbox_entry.t String.Table.t
  ; idempotency : idem String.Table.t
  ; mutable audit : Audit.t list (* newest first *)
  ; ledger : Ledger_posting.t list Int.Table.t (* by journal seq *)
  ; mutable seq : int
  }

let kind = "memory"

let create () =
  { customers = String.Table.create ()
  ; emails = String.Table.create ()
  ; provider_refs = String.Table.create ()
  ; sessions = String.Table.create ()
  ; kyc = String.Table.create ()
  ; accounts = String.Table.create ()
  ; beneficiaries = String.Table.create ()
  ; transfers = String.Table.create ()
  ; swaps = String.Table.create ()
  ; cards = String.Table.create ()
  ; card_requests = String.Table.create ()
  ; standing_orders = String.Table.create ()
  ; reviews = String.Table.create ()
  ; outbox = String.Table.create ()
  ; inbox = String.Table.create ()
  ; idempotency = String.Table.create ()
  ; audit = []
  ; ledger = Int.Table.create ()
  ; seq = 0
  }
;;

let next_seq t =
  t.seq <- t.seq + 1;
  t.seq
;;

let put_aged t table key value =
  Hashtbl.update table key ~f:(function
    | Some { seq; value = _ } -> { seq; value }
    | None -> { seq = next_seq t; value })
;;

let values_by_age table ~f =
  Hashtbl.data table
  |> List.filter ~f:(fun a -> f a.value)
  |> List.sort ~compare:(fun a b -> Int.compare a.seq b.seq)
  |> List.map ~f:(fun a -> a.value)
;;

let cid = Customer_id.to_string

(* Every check runs before anything is written, so a batch lands whole or not at all. *)
let check t writes =
  List.fold_until
    writes
    ~init:()
    ~finish:(fun () -> Ok Commit_result.Committed)
    ~f:(fun () (w : Write.t) : (_, Commit_result.t Or_error.t) Continue_or_stop.t ->
      match w with
      | Insert_customer c ->
        if Hashtbl.mem t.customers (cid c.id)
        then Stop (Ok (Commit_result.Conflict "customer_exists"))
        else if Hashtbl.mem t.emails c.email
        then Stop (Ok (Commit_result.Conflict "email_taken"))
        else Continue ()
      | Insert_card c ->
        if Hashtbl.mem t.card_requests c.request_id
        then Stop (Ok (Commit_result.Conflict "card_request_exists"))
        else Continue ()
      | Inbox e ->
        (match Hashtbl.find t.inbox (Banking_provider.Event_id.to_string e.event_id) with
         | None -> Continue ()
         | Some old when String.equal old.payload_hash e.payload_hash ->
           Stop (Ok Commit_result.Duplicate_event)
         | Some _ ->
           Stop
             (Or_error.error_s
                [%message
                  "provider event id reused with different content"
                    ~event_id:(e.event_id : Banking_provider.Event_id.t)]))
      | Update_customer c ->
        (match Hashtbl.find t.customers (cid c.id) with
         | None ->
           Stop (Or_error.error_s [%message "no such customer" (c.id : Customer_id.t)])
         | Some old when not (String.equal old.email c.email) ->
           Stop (Or_error.error_string "the e-mail address cannot change")
         | Some _ -> Continue ())
      | Update_outbox m ->
        if Hashtbl.mem t.outbox m.id
        then Continue ()
        else Stop (Or_error.error_s [%message "no such outbox message" m.id])
      | Put_session _
      | Delete_session _
      | Kyc_event _
      | Put_account _
      | Put_beneficiary _
      | Put_transfer _
      | Put_swap _
      | Put_card _
      | Put_standing_order _
      | Put_review _
      | Enqueue _
      | Audit _
      | Ledger_record _ -> Continue ())
;;

let apply t (w : Write.t) =
  match w with
  | Insert_customer c | Update_customer c ->
    Hashtbl.set t.customers ~key:(cid c.id) ~data:c;
    Hashtbl.set t.emails ~key:c.email ~data:(cid c.id);
    Option.iter c.provider_ref ~f:(fun r ->
      Hashtbl.set t.provider_refs ~key:(Customer_ref.to_string r) ~data:(cid c.id))
  | Put_session s -> Hashtbl.set t.sessions ~key:s.token_hash ~data:s
  | Delete_session { token_hash } -> Hashtbl.remove t.sessions token_hash
  | Kyc_event e ->
    (* An event id is appended once per customer, like the unique key in Postgres. *)
    if not
         (List.exists
            (Hashtbl.find_multi t.kyc (cid e.customer_id))
            ~f:(fun old -> Kyc_core.Event_id.equal old.id e.id))
    then Hashtbl.add_multi t.kyc ~key:(cid e.customer_id) ~data:e
  | Put_account a ->
    Hashtbl.set t.accounts ~key:(Account_ref.to_string (Account.id a)) ~data:a
  | Put_beneficiary b ->
    put_aged t t.beneficiaries (Payments_core.Beneficiary.Id.to_string b.id) b
  | Put_transfer x ->
    put_aged t t.transfers (Payments_core.Transfer.Id.to_string (Transfer.id x)) x
  | Put_swap s -> put_aged t t.swaps (Payments_core.Swap.Id.to_string s.id) s
  | Insert_card c | Put_card c ->
    Hashtbl.set
      t.card_requests
      ~key:c.request_id
      ~data:(Card_core.Ids.Card_id.to_string c.id);
    put_aged t t.cards (Card_core.Ids.Card_id.to_string c.id) c
  | Put_standing_order o ->
    put_aged
      t
      t.standing_orders
      (Payments_core.Standing_order.Id.to_string (Standing_order.id o))
      o
  | Put_review r -> put_aged t t.reviews r.id r
  | Enqueue m | Update_outbox m -> put_aged t t.outbox m.id m
  | Inbox e ->
    Hashtbl.set t.inbox ~key:(Banking_provider.Event_id.to_string e.event_id) ~data:e
  | Ledger_record r ->
    if not (Hashtbl.mem t.ledger r.seq)
    then Hashtbl.set t.ledger ~key:r.seq ~data:(Ledger_posting.of_record r)
  | Audit record ->
    let prev_hash, seq =
      match t.audit with
      | [] -> Audit.genesis_hash, 1
      | last :: _ -> last.hash, last.seq + 1
    in
    t.audit
    <- { seq; record; prev_hash; hash = Audit.chain_hash ~prev_hash record } :: t.audit
;;

let commit t writes =
  return
    (match check t writes with
     | Ok Committed ->
       List.iter writes ~f:(apply t);
       Ok Commit_result.Committed
     | result -> result)
;;

let found x = return (Ok x)
let customer t id = found (Hashtbl.find t.customers (cid id))

let customer_by_email t email =
  found
    (Option.bind
       (Hashtbl.find t.emails (Customer.normalize_email email))
       ~f:(Hashtbl.find t.customers))
;;

let customer_by_provider_ref t r =
  found
    (Option.bind
       (Hashtbl.find t.provider_refs (Customer_ref.to_string r))
       ~f:(Hashtbl.find t.customers))
;;

let session t ~token_hash = found (Hashtbl.find t.sessions token_hash)
let kyc_events t id = found (List.rev (Hashtbl.find_multi t.kyc (cid id)))
let account t id = found (Hashtbl.find t.accounts (Account_ref.to_string id))

let accounts t id =
  found
    (Hashtbl.data t.accounts
     |> List.filter ~f:(fun (a : Account.t) -> Customer_id.equal a.customer id)
     |> List.sort ~compare:(fun a b ->
       (* Byte order of the text forms, as Postgres sorts with COLLATE "C". *)
       Comparable.lift
         [%compare: string * string]
         ~f:(fun a ->
           ( Money.Currency.to_string (Account.currency a)
           , Account_ref.to_string (Account.id a) ))
         a
         b))
;;

let find_aged table key = Option.map (Hashtbl.find table key) ~f:(fun a -> a.value)

let beneficiary t id =
  found (find_aged t.beneficiaries (Payments_core.Beneficiary.Id.to_string id))
;;

let beneficiaries t id =
  found
    (values_by_age t.beneficiaries ~f:(fun (b : Payments_core.Beneficiary.t) ->
       String.equal b.owner (cid id)))
;;

let transfer t id = found (find_aged t.transfers (Payments_core.Transfer.Id.to_string id))

let transfers t id ?since () =
  found
    (values_by_age t.transfers ~f:(fun (x : Transfer.t) ->
       Customer_id.equal x.customer id
       &&
       match since with
       | None -> true
       | Some since -> Time_ns.( >= ) x.created_at since)
     |> List.rev)
;;

let swap t id = found (find_aged t.swaps (Payments_core.Swap.Id.to_string id))

let swaps t id =
  found
    (values_by_age t.swaps ~f:(fun (s : Swap.t) -> Customer_id.equal s.customer id)
     |> List.rev)
;;

let card t id = found (find_aged t.cards (Card_core.Ids.Card_id.to_string id))

let card_by_request t ~request_id =
  found (Option.bind (Hashtbl.find t.card_requests request_id) ~f:(find_aged t.cards))
;;

let cards t id =
  found (values_by_age t.cards ~f:(fun (c : Card.t) -> Customer_id.equal c.customer id))
;;

let standing_order t id =
  found (find_aged t.standing_orders (Payments_core.Standing_order.Id.to_string id))
;;

let standing_order_by_provider_ref t r =
  found
    (values_by_age t.standing_orders ~f:(fun (o : Standing_order.t) ->
       Option.equal Banking_provider.Standing_order_ref.equal o.provider_ref (Some r))
     |> List.hd)
;;

let standing_orders t id =
  found
    (values_by_age t.standing_orders ~f:(fun (o : Standing_order.t) ->
       Customer_id.equal o.customer id))
;;

let review t id = found (find_aged t.reviews id)

let reviews t ~pending_only =
  found (values_by_age t.reviews ~f:(fun r -> (not pending_only) || Review.is_pending r))
;;

let outbox_due t ~now ~limit =
  found
    (values_by_age t.outbox ~f:(fun (m : Outbox.t) ->
       match m.state with
       | Pending -> Time_ns.( <= ) m.next_attempt_at now
       | Sent _ | Needs_review -> false)
     |> fun l -> List.take l limit)
;;

let outbox t id = found (find_aged t.outbox id)

let outbox_in_state t ~state =
  found
    (values_by_age t.outbox ~f:(fun (m : Outbox.t) ->
       String.equal (Outbox.State.name m.state) state))
;;

let inbox_mem t id = found (Hashtbl.mem t.inbox (Banking_provider.Event_id.to_string id))

let idempotency_begin t scope ~request_hash ~now =
  let key = Idempotency.Scope.to_string scope in
  found
    (match Hashtbl.find t.idempotency key with
     | None ->
       Hashtbl.set
         t.idempotency
         ~key
         ~data:{ request_hash; completed = None; created_at = now };
       Idempotency.Begin.Started
     | Some i when not (String.equal i.request_hash request_hash) -> Mismatch
     | Some { completed = None; _ } -> In_progress
     | Some { completed = Some (status, body); _ } -> Replay { status; body })
;;

let idempotency_complete t scope ~status ~body =
  let key = Idempotency.Scope.to_string scope in
  return
    (match Hashtbl.find t.idempotency key with
     | Some i ->
       i.completed <- Some (status, body);
       Ok ()
     | None -> Or_error.error_s [%message "no request under this idempotency key" key])
;;

let idempotency_abandon t scope =
  Hashtbl.remove t.idempotency (Idempotency.Scope.to_string scope);
  return (Ok ())
;;

let audit_log t = found (List.rev t.audit)

let ledger_head t =
  found
    (Hashtbl.keys t.ledger |> List.max_elt ~compare:Int.compare |> Option.value ~default:0)
;;

let all_postings t =
  Hashtbl.to_alist t.ledger
  |> List.sort ~compare:(fun (a, _) (b, _) -> Int.compare a b)
  |> List.concat_map ~f:snd
;;

let ledger_postings t ~account =
  found
    (List.filter (all_postings t) ~f:(fun (p : Ledger_posting.t) ->
       String.equal p.account account))
;;

let ledger_trial_balance t =
  let sums = String.Table.create () in
  List.iter (all_postings t) ~f:(fun p ->
    let signed = Ledger_posting.signed p in
    Hashtbl.update
      sums
      (Money.Currency.to_string (Money.Amount.currency signed))
      ~f:(function
        | None -> signed
        | Some sum -> Money.Amount.add_exn sum signed));
  found
    (Hashtbl.to_alist sums
     |> List.sort ~compare:(fun (a, _) (b, _) -> String.compare a b)
     |> List.map ~f:snd)
;;

let prune t ~idempotency_before ~inbox_before ~sessions_before =
  let drop table ~f =
    let before = Hashtbl.length table in
    Hashtbl.filter_inplace table ~f:(Fn.non f);
    before - Hashtbl.length table
  in
  let idempotency_keys =
    drop t.idempotency ~f:(fun i -> Time_ns.( < ) i.created_at idempotency_before)
  in
  let inbox_rows =
    drop t.inbox ~f:(fun (e : Inbox_entry.t) -> Time_ns.( < ) e.received_at inbox_before)
  in
  let sessions =
    drop t.sessions ~f:(fun (s : Session.t) ->
      Time_ns.( < ) s.last_activity sessions_before)
  in
  found { Store.Pruned.idempotency_keys; inbox_rows; sessions }
;;
