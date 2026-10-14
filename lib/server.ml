open! Core
open! Async
open Neobank_api_types
open Model
module A = Api_types

module Config = struct
  type t =
    { allowed_origins : string list
    ; max_body : int
    }

  let default = { allowed_origins = [ "http://localhost:8000" ]; max_body = 65_536 }
end

type t =
  { ctx : Ctx.t
  ; config : Config.t
  ; mutable ws_closers : (unit -> unit) list
  }

let create ?(config = Config.default) ctx = { ctx; config; ws_closers = [] }
let ctx t = t.ctx

(* Replies *)

module Reply = struct
  type t =
    { status : Cohttp.Code.status_code
    ; body : string
    ; content_type : string
    ; headers : (string * string) list
    }

  let json ?(status = `OK) ?(headers = []) json =
    { status
    ; body = Yojson.Safe.to_string json
    ; content_type = "application/json"
    ; headers
    }
  ;;

  let text ?(content_type = "text/plain; version=0.0.4") body =
    { status = `OK; body; content_type; headers = [] }
  ;;

  let empty status =
    { status; body = ""; content_type = "application/json"; headers = [] }
  ;;

  let problem (p : Problem.t) =
    match p.sca with
    | Some challenge ->
      json
        ~status:p.status
        (A.Sca_required.yojson_of_t { error = p.code; message = p.message; challenge })
    | None ->
      json
        ~status:p.status
        (A.Api_error.yojson_of_t { error = p.code; message = p.message })
  ;;
end

(* Requests *)

type request =
  { meth : Cohttp.Code.meth
  ; path : string list
  ; uri : Uri.t
  ; headers : Cohttp.Header.t
  ; body : string
  }

let header (r : request) name = Cohttp.Header.get r.headers name
let query (r : request) name = Uri.get_query_param r.uri name
let ( let* ) = Deferred.Result.( >>= )
let ok = Deferred.Result.return
let fail p = return (Error p)

let parse_json body =
  if String.is_empty (String.strip body)
  then Ok (`Assoc [])
  else (
    try Ok (Yojson.Safe.from_string body) with
    | Yojson.Json_error _ ->
      Error (Problem.bad_request "invalid_json" "the body is not JSON"))
;;

(* ppx_yojson_conv messages embed the source path of the type; keep only what names the
   offending field. *)
let decode of_yojson (r : request) =
  return
    (Result.bind (parse_json r.body) ~f:(fun json ->
       try Ok (of_yojson json) with
       | Ppx_yojson_conv_lib.Yojson_conv.Of_yojson_error (exn, _) ->
         let msg =
           match exn with
           | Failure msg -> msg
           | exn -> Exn.to_string exn
         in
         let msg =
           match String.rsplit2 msg ~on:':' with
           | Some (_, detail) -> String.strip detail
           | None -> msg
         in
         Error (Problem.bad_request "invalid_request" ("invalid request body: " ^ msg))
       | _ -> Error (Problem.bad_request "invalid_request" "invalid request body")))
;;

let rec canonical (json : Yojson.Safe.t) : Yojson.Safe.t =
  match json with
  | `Assoc fields ->
    `Assoc
      (List.map fields ~f:(fun (k, v) -> k, canonical v)
       |> List.sort ~compare:(fun (a, _) (b, _) -> String.compare a b))
  | `List l -> `List (List.map l ~f:canonical)
  | other -> other
;;

let request_hash body =
  let text =
    match parse_json body with
    | Ok json -> Yojson.Safe.to_string (canonical json)
    | Error _ -> body
  in
  Cryptokit.transform_string
    (Cryptokit.Hexa.encode ())
    (Cryptokit.hash_string (Cryptokit.Hash.sha256 ()) text)
;;

(* Routes *)

type who =
  | Anonymous
  | Customer of Session.t
  | Ops

type access =
  | Public
  | Signed_in
  | Operator

type route =
  { name : string
  ; access : access
  ; idempotent : bool
  ; run : who:who -> idem_key:string option -> Reply.t Problem.res
  }

let customer_of = function
  | Customer s -> s
  | Anonymous | Ops -> failwith "route without a customer session"
;;

let json_list f l = `List (List.map l ~f)

let parse_time s =
  match Or_error.try_with (fun () -> Date.of_string s) with
  | Ok date ->
    Ok (Time_ns.of_date_ofday ~zone:Time_float.Zone.utc date Time_ns.Ofday.start_of_day)
  | Error _ ->
    Or_error.try_with (fun () ->
      Time_ns.of_string_with_utc_offset (String.tr s ~target:'T' ~replacement:' '))
    |> Result.map_error ~f:(fun _ ->
      Problem.bad_request "invalid_request" "times are YYYY-MM-DD or ISO 8601 with a zone")
;;

let me (ctx : Ctx.t) (s : Session.t) =
  let* c = Ctx.find ~what:"customer" (Any_store.customer ctx.store s.customer) in
  let* kyc = Onboarding.status ctx s.customer in
  let* accounts = Ctx.read (Any_store.accounts ctx.store s.customer) in
  ok
    { A.Me.customer_id = Customer_id.to_string c.id
    ; email = c.email
    ; kyc
    ; accounts = List.length accounts
    ; sca_fresh = Auth.sca_is_fresh ctx s
    }
;;

let route t (r : request) : route =
  let ctx = t.ctx in
  let public ?(idempotent = false) name run =
    { name; access = Public; idempotent; run }
  in
  let signed_in ?(idempotent = true) name f =
    { name
    ; access = Signed_in
    ; idempotent
    ; run = (fun ~who ~idem_key -> f (customer_of who) ~idem_key)
    }
  in
  let operator name f =
    { name; access = Operator; idempotent = true; run = (fun ~who:_ ~idem_key:_ -> f ()) }
  in
  let get name f = signed_in ~idempotent:false name (fun s ~idem_key:_ -> f s) in
  let post name f = signed_in name (fun s ~idem_key:_ -> f s) in
  let reply ?status json = ok (Reply.json ?status json) in
  match r.meth, r.path with
  | `GET, [ "healthz" ] ->
    public "/healthz" (fun ~who:_ ~idem_key:_ ->
      reply
        (`Assoc
          [ "status", `String "ok"
          ; "store", `String (Any_store.kind_of ctx.store)
          ; "provider", `String (Bank.name ctx.bank)
          ; "sandbox", `Bool (Bank.has_sandbox ctx.bank)
          ]))
  | `GET, [ "metrics" ] ->
    public "/metrics" (fun ~who:_ ~idem_key:_ ->
      ok (Reply.text (Metrics.render ctx.metrics)))
  | `POST, [ "auth"; "signup" ] ->
    public "/auth/signup" (fun ~who:_ ~idem_key:_ ->
      let* body = decode A.Auth.Signup_request.t_of_yojson r in
      let* response = Auth.signup ctx body in
      reply ~status:`Created (A.Auth.Signup_response.yojson_of_t response))
  | `POST, [ "auth"; "login" ] ->
    public "/auth/login" (fun ~who:_ ~idem_key:_ ->
      let* body = decode A.Auth.Login_request.t_of_yojson r in
      let* session = Auth.login ctx body in
      reply (A.Auth.Session.yojson_of_t session))
  | `POST, [ "auth"; "step-up" ] ->
    signed_in ~idempotent:false "/auth/step-up" (fun s ~idem_key:_ ->
      let* body = decode A.Auth.Step_up_request.t_of_yojson r in
      let* session = Auth.step_up ctx s body in
      reply (A.Auth.Session.yojson_of_t session))
  | `POST, [ "auth"; "logout" ] ->
    signed_in ~idempotent:false "/auth/logout" (fun s ~idem_key:_ ->
      let* () = Auth.logout ctx s in
      ok (Reply.empty `No_content))
  | `GET, [ "me" ] ->
    get "/me" (fun s ->
      let* me = me ctx s in
      reply (A.Me.yojson_of_t me))
  | `GET, [ "kyc" ] ->
    get "/kyc" (fun s ->
      let* status = Onboarding.status ctx s.customer in
      reply (A.Kyc.Status.yojson_of_t status))
  | `POST, [ "kyc"; "start" ] ->
    post "/kyc/start" (fun s ->
      let* body = decode A.Kyc.Start_request.t_of_yojson r in
      let* c = Ctx.find ~what:"customer" (Any_store.customer ctx.store s.customer) in
      let* session = Onboarding.start ctx c body in
      reply (A.Kyc.Session.yojson_of_t session))
  | `POST, [ "kyc"; "complete" ] ->
    post "/kyc/complete" (fun s ->
      let* body = decode A.Kyc.Complete_request.t_of_yojson r in
      let* c = Ctx.find ~what:"customer" (Any_store.customer ctx.store s.customer) in
      let* status = Onboarding.complete ctx c body in
      reply (A.Kyc.Status.yojson_of_t status))
  | `GET, [ "accounts" ] ->
    get "/accounts" (fun s ->
      let* l = Accounts.list ctx s.customer in
      reply (json_list A.Account.yojson_of_t l))
  | `GET, [ "accounts"; id ] ->
    get "/accounts/:id" (fun s ->
      let* a = Accounts.get ctx s.customer id in
      reply (A.Account.yojson_of_t a))
  | `GET, [ "accounts"; id; "statement" ] ->
    get "/accounts/:id/statement" (fun s ->
      let time name =
        match query r name with
        | None -> Ok None
        | Some v -> Result.map (parse_time v) ~f:Option.some
      in
      let* from = return (time "from") in
      let* until = return (time "until") in
      let* statement = Accounts.statement ctx s id ~from ~until in
      match query r "format" with
      | Some "csv" ->
        ok (Reply.text ~content_type:"text/csv" (Accounts.statement_csv statement))
      | _ -> reply (A.Statement.yojson_of_t (Views.statement statement)))
  | `GET, [ "beneficiaries" ] ->
    get "/beneficiaries" (fun s ->
      let* l = Payments.beneficiaries ctx s.customer in
      reply (json_list A.Beneficiary.yojson_of_t l))
  | `POST, [ "beneficiaries" ] ->
    post "/beneficiaries" (fun s ->
      let* body = decode A.Beneficiary.Request.t_of_yojson r in
      let* b = Payments.add_beneficiary ctx s.customer body in
      reply ~status:`Created (A.Beneficiary.yojson_of_t b))
  | `POST, [ "beneficiaries"; id; "trust" ] ->
    post "/beneficiaries/:id/trust" (fun s ->
      let* b = Payments.trust_beneficiary ctx s id in
      reply (A.Beneficiary.yojson_of_t b))
  | `POST, [ "transfers"; "quote" ] ->
    post "/transfers/quote" (fun s ->
      let* body = decode A.Fee_quote.Request.t_of_yojson r in
      let* q = Payments.quote_fee ctx s.customer body in
      reply (A.Fee_quote.yojson_of_t q))
  | `POST, [ "transfers" ] ->
    post "/transfers" (fun s ->
      let* body = decode A.Transfer.Request.t_of_yojson r in
      let* status, x =
        Payments.create_transfer ctx s ~wallet_provider:body.wallet_provider body
      in
      reply ~status (A.Transfer.yojson_of_t x))
  | `GET, [ "transfers" ] ->
    get "/transfers" (fun s ->
      let* l = Payments.list_transfers ctx s.customer in
      reply (json_list A.Transfer.yojson_of_t l))
  | `GET, [ "transfers"; id ] ->
    get "/transfers/:id" (fun s ->
      let* x = Payments.get_transfer ctx s.customer id in
      reply (A.Transfer.yojson_of_t x))
  | `POST, [ "transfers"; id; "confirm-sca" ] ->
    post "/transfers/:id/confirm-sca" (fun s ->
      let* body = decode A.Transfer.Confirm_request.t_of_yojson r in
      let* x = Payments.confirm_sca ctx s.customer id body in
      reply (A.Transfer.yojson_of_t x))
  | `POST, [ "transfers"; id; "sca" ] ->
    post "/transfers/:id/sca" (fun s ->
      let* x = Payments.restart_sca ctx s.customer id in
      reply (A.Transfer.yojson_of_t x))
  | `POST, [ "transfers"; id; "cancel" ] ->
    post "/transfers/:id/cancel" (fun s ->
      let* x = Payments.cancel_transfer ctx s.customer id in
      reply (A.Transfer.yojson_of_t x))
  | `POST, [ "swaps"; "quote" ] ->
    post "/swaps/quote" (fun s ->
      let* body = decode A.Swap.Quote_request.t_of_yojson r in
      let* x = Swaps.quote ctx s body in
      reply ~status:`Created (A.Swap.yojson_of_t x))
  | `POST, [ "swaps"; id; "execute" ] ->
    post "/swaps/:id/execute" (fun s ->
      let* x = Swaps.execute ctx s id in
      reply (A.Swap.yojson_of_t x))
  | `GET, [ "swaps" ] ->
    get "/swaps" (fun s ->
      let* l = Swaps.list ctx s.customer in
      reply (json_list A.Swap.yojson_of_t l))
  | `GET, [ "swaps"; id ] ->
    get "/swaps/:id" (fun s ->
      let* x = Swaps.get ctx s.customer id in
      reply (A.Swap.yojson_of_t x))
  | `GET, [ "cards" ] ->
    get "/cards" (fun s ->
      let* l = Cards.list ctx s.customer in
      reply (json_list A.Card.yojson_of_t l))
  | `POST, [ "cards" ] ->
    signed_in "/cards" (fun s ~idem_key ->
      let* body = decode A.Card.Issue_request.t_of_yojson r in
      (* The provider's request id comes from the client's idempotency key, so a retry
         after a lost answer finds the card the first attempt issued. *)
      let request_id =
        Customer_id.to_string s.customer ^ ":" ^ Option.value idem_key ~default:""
      in
      let* c = Cards.issue ctx s ~request_id body in
      reply ~status:`Created (A.Card.yojson_of_t c))
  | `GET, [ "cards"; id ] ->
    get "/cards/:id" (fun s ->
      let* c = Cards.get ctx s.customer id in
      reply (A.Card.yojson_of_t c))
  | `POST, [ "cards"; id; "freeze" ] ->
    post "/cards/:id/freeze" (fun s ->
      let* c = Cards.freeze ctx s.customer id in
      reply (A.Card.yojson_of_t c))
  | `POST, [ "cards"; id; "unfreeze" ] ->
    post "/cards/:id/unfreeze" (fun s ->
      let* c = Cards.unfreeze ctx s.customer id in
      reply (A.Card.yojson_of_t c))
  | (`POST | `PUT), [ "cards"; id; "limits" ] ->
    post "/cards/:id/limits" (fun s ->
      let* body = decode A.Card.Limits.t_of_yojson r in
      let* c = Cards.set_limits ctx s id body in
      reply (A.Card.yojson_of_t c))
  | `POST, [ "cards"; id; "pan-token" ] ->
    post "/cards/:id/pan-token" (fun s ->
      match%bind Cards.pan_token ctx s id with
      | Error p -> fail p
      | Ok (`Token token) -> reply (A.Card.Pan_token.yojson_of_t token)
      | Ok (`Challenge challenge) ->
        reply
          ~status:`Accepted
          (A.Sca_required.yojson_of_t
             { error = "sca_required"
             ; message = "confirm with the bank's code (POST /sca/:id), then ask again"
             ; challenge
             }))
  | `GET, [ "standing-orders" ] ->
    get "/standing-orders" (fun s ->
      let* l = Standing_orders.list ctx s.customer in
      reply (json_list A.Standing_order.yojson_of_t l))
  | `POST, [ "standing-orders" ] ->
    post "/standing-orders" (fun s ->
      let* body = decode A.Standing_order.Request.t_of_yojson r in
      let* o = Standing_orders.create ctx s body in
      reply ~status:`Created (A.Standing_order.yojson_of_t o))
  | `DELETE, [ "standing-orders"; id ] | `POST, [ "standing-orders"; id; "cancel" ] ->
    signed_in
      ~idempotent:(Poly.equal r.meth `POST)
      "/standing-orders/:id/cancel"
      (fun s ~idem_key:_ ->
         let* o = Standing_orders.cancel ctx s.customer id in
         reply (A.Standing_order.yojson_of_t o))
  | `POST, [ "sca"; id ] ->
    post "/sca/:id" (fun s ->
      let* body = decode A.Sca_confirm_request.t_of_yojson r in
      let* `Accepted = Provider_sca.confirm ctx s.customer id body in
      reply (`Assoc [ "status", `String "accepted" ]))
  | `POST, [ "sandbox"; "kyc-decision" ] ->
    post "/sandbox/kyc-decision" (fun s ->
      let* body = decode A.Sandbox.Kyc_decision.t_of_yojson r in
      let* status = Sandbox_api.kyc_decision ctx s.customer body in
      reply (A.Kyc.Status.yojson_of_t status))
  | `POST, [ "sandbox"; "deposit" ] ->
    post "/sandbox/deposit" (fun s ->
      let* body = decode A.Sandbox.Deposit.t_of_yojson r in
      let* id = Sandbox_api.deposit ctx s.customer body in
      reply ~status:`Created (`Assoc [ "transfer_id", `String id ]))
  | `POST, [ "sandbox"; "card-auth" ] ->
    post "/sandbox/card-auth" (fun s ->
      let* body = decode A.Sandbox.Card_auth.t_of_yojson r in
      let* result = Sandbox_api.card_auth ctx s.customer body in
      reply (A.Sandbox.Card_auth_result.yojson_of_t result))
  | `POST, [ "sandbox"; "transfer-outcome" ] ->
    post "/sandbox/transfer-outcome" (fun s ->
      let* body = decode A.Sandbox.Transfer_outcome.t_of_yojson r in
      let* x = Sandbox_api.transfer_outcome ctx s.customer body in
      reply (A.Transfer.yojson_of_t x))
  | `POST, [ "webhooks"; "provider" ] ->
    public "/webhooks/provider" (fun ~who:_ ~idem_key:_ ->
      let request =
        { Banking_provider.Webhook.Request.headers = Cohttp.Header.to_list r.headers
        ; body = r.body
        }
      in
      let* applied, duplicates = Events.ingest_webhook ctx request in
      reply (`Assoc [ "applied", `Int applied; "duplicates", `Int duplicates ]))
  | `GET, [ "ops"; "review" ] ->
    { (operator "/ops/review" (fun () ->
         let pending_only =
           not (Option.equal String.equal (query r "all") (Some "true"))
         in
         let* l = Payments.reviews ctx ~pending_only in
         reply (json_list A.Review.yojson_of_t l)))
      with
      idempotent = false
    }
  | `POST, [ "ops"; "review"; id ] ->
    operator "/ops/review/:id" (fun () ->
      let* body = decode A.Review.Decide_request.t_of_yojson r in
      let* review = Payments.decide_review ctx id body in
      reply (A.Review.yojson_of_t review))
  | `GET, [ "ops"; "outbox" ] ->
    { (operator "/ops/outbox" (fun () ->
         let state = Option.value (query r "state") ~default:"needs_review" in
         let* l = Ctx.read (Any_store.outbox_in_state ctx.store ~state) in
         reply
           (json_list
              (fun (m : Outbox.t) ->
                `Assoc
                  [ "id", `String m.id
                  ; "kind", `String (Outbox.Kind.name m.kind)
                  ; "aggregate_id", `String (Outbox.Kind.aggregate_id m.kind)
                  ; "attempts", `Int m.attempts
                  ; "state", `String (Outbox.State.name m.state)
                  ; ( "last_error"
                    , Option.value_map m.last_error ~default:`Null ~f:(fun e -> `String e)
                    )
                  ; "created_at", `String (Views.time m.created_at)
                  ])
              l)))
      with
      idempotent = false
    }
  | `GET, [ "ops"; "ledger" ] ->
    { (operator "/ops/ledger" (fun () ->
         let* head = Ctx.read (Any_store.ledger_head ctx.store) in
         let* trial = Ctx.read (Any_store.ledger_trial_balance ctx.store) in
         reply
           (`Assoc
             [ "head", `Int head
             ; ( "trial_balance"
               , json_list (fun a -> A.Amount.yojson_of_t (Views.amount a)) trial )
             ; "balanced", `Bool (List.for_all trial ~f:Money.Amount.is_zero)
             ])))
      with
      idempotent = false
    }
  | `GET, [ "ops"; "ledger"; "postings" ] ->
    { (operator "/ops/ledger/postings" (fun () ->
         match query r "account" with
         | None -> fail (Problem.bad_request "invalid_request" "account is required")
         | Some account ->
           let* postings = Ctx.read (Any_store.ledger_postings ctx.store ~account) in
           reply
             (json_list
                (fun (p : Ledger_posting.t) ->
                  `Assoc
                    [ "seq", `Int p.seq
                    ; "line", `Int p.line
                    ; ( "side"
                      , `String
                          (String.lowercase
                             (Sexp.to_string [%sexp (p.side : Ledger.Side.t)])) )
                    ; "amount", A.Amount.yojson_of_t (Views.amount p.amount)
                    ; "code", `String p.code
                    ; "reference", `String p.reference
                    ; "recorded_at", `String (Views.time p.recorded_at)
                    ])
                postings)))
      with
      idempotent = false
    }
  | `GET, [ "ops"; "audit" ] ->
    { (operator "/ops/audit" (fun () ->
         let* log = Ctx.read (Any_store.audit_log ctx.store) in
         let chain =
           match Audit.verify_chain log with
           | Ok () -> `String "ok"
           | Error e -> `String (Error.to_string_hum e)
         in
         let latest = List.rev (List.take (List.rev log) 50) in
         reply
           (`Assoc
             [ "chain", chain
             ; "records", `Int (List.length log)
             ; ( "latest"
               , json_list
                   (fun (a : Audit.t) ->
                     `Assoc
                       [ "seq", `Int a.seq
                       ; "at", `String (Views.time a.record.at)
                       ; "actor", `String a.record.actor
                       ; "action", `String a.record.action
                       ; "object_id", `String a.record.object_id
                       ; "outcome", `String a.record.outcome
                       ; "hash", `String a.hash
                       ])
                   latest )
             ])))
      with
      idempotent = false
    }
  | _ -> public "unknown" (fun ~who:_ ~idem_key:_ -> fail (Problem.not_found "route"))
;;

(* Authentication *)

let bearer (r : request) =
  Option.bind (header r "authorization") ~f:(fun v ->
    match String.lsplit2 (String.strip v) ~on:' ' with
    | Some (scheme, token) when String.Caseless.equal scheme "bearer" ->
      Some (String.strip token)
    | _ -> None)
;;

let constant_time_equal a b =
  String.length a = String.length b
  && String.foldi a ~init:0 ~f:(fun i acc c ->
       acc lor (Char.to_int c lxor Char.to_int b.[i]))
     = 0
;;

let authorize t (r : request) access : who Problem.res =
  match access with
  | Public -> ok Anonymous
  | Signed_in ->
    (match bearer r with
     | None -> fail (Problem.unauthorized "no_session" "missing session token")
     | Some token ->
       let* s = Auth.authenticate t.ctx ~token in
       ok (Customer s))
  | Operator ->
    (match t.ctx.config.ops_token, bearer r with
     | None, _ -> fail (Problem.not_found "route")
     | Some expected, Some given when constant_time_equal expected given -> ok Ops
     | Some _, _ -> fail (Problem.unauthorized "not_operator" "operator token required"))
;;

(* Idempotency keys *)

let valid_key k =
  String.length k >= 1
  && String.length k <= 255
  && String.for_all k ~f:(fun c -> Char.to_int c >= 0x21 && Char.to_int c <= 0x7e)
;;

let stored_status = function
  (* Malformed requests and authentication failures never executed; a corrected retry may
     reuse the key. Everything else, server errors included, is replayed. *)
  | `Bad_request | `Unauthorized | `Forbidden -> false
  | _ -> true
;;

let with_idempotency t (r : request) (rt : route) ~who =
  let run idem_key =
    Deferred.map (rt.run ~who ~idem_key) ~f:(function
      | Ok reply -> reply
      | Error p -> Reply.problem p)
  in
  if not (rt.idempotent && Poly.equal r.meth `POST)
  then run None
  else (
    match header r "idempotency-key" with
    | None ->
      return
        (Reply.problem
           (Problem.bad_request
              "idempotency_key_required"
              "POST requests need an Idempotency-Key header"))
    | Some key when not (valid_key key) ->
      return
        (Reply.problem
           (Problem.bad_request
              "invalid_idempotency_key"
              "Idempotency-Key must be 1 to 255 printable ASCII characters"))
    | Some key ->
      let scope : Idempotency.Scope.t =
        { customer =
            (match who with
             | Customer s -> Customer_id.to_string s.customer
             | Ops -> "ops"
             | Anonymous -> "anonymous")
        ; meth = "POST"
        ; route = rt.name
        ; key
        }
      in
      let store = t.ctx.store in
      (match%bind
         Any_store.idempotency_begin
           store
           scope
           ~request_hash:(request_hash r.body)
           ~now:(Ctx.now t.ctx)
       with
       | Error e -> return (Reply.problem (Problem.internal e))
       | Ok Mismatch ->
         return
           (Reply.problem
              (Problem.unprocessable
                 "idempotency_key_reused"
                 "this Idempotency-Key was used with a different body"))
       | Ok In_progress ->
         return
           (Reply.problem
              (Problem.conflict
                 "idempotency_key_in_use"
                 "a request with this Idempotency-Key is still running"))
       | Ok (Replay { status; body }) ->
         Metrics.incr t.ctx.metrics "idempotent_replays";
         return
           { Reply.status = Cohttp.Code.status_of_code status
           ; body
           ; content_type = "application/json"
           ; headers = [ "idempotent-replayed", "true" ]
           }
       | Ok Started ->
         let%bind reply =
           Monitor.try_with ~extract_exn:true (fun () -> run (Some key))
           >>| function
           | Ok reply -> reply
           | Error exn ->
             Log.Global.error_s [%message "handler failed" (exn : exn)];
             Reply.problem (Problem.internal (Error.of_exn exn))
         in
         let%map result =
           if stored_status reply.status
           then
             Any_store.idempotency_complete
               store
               scope
               ~status:(Cohttp.Code.code_of_status reply.status)
               ~body:reply.body
           else Any_store.idempotency_abandon store scope
         in
         (match result with
          | Ok () -> ()
          | Error e ->
            Log.Global.error_s [%message "idempotency record failed" (e : Error.t)]);
         reply))
;;

(* Websocket *)

let header_has (r : request) name value =
  match header r name with
  | Some v ->
    String.split v ~on:','
    |> List.exists ~f:(fun v -> String.Caseless.equal (String.strip v) value)
  | None -> false
;;

let ws_upgrade t (r : request) (s : Session.t) =
  match header r "sec-websocket-key" with
  | None -> Error (Problem.bad_request "invalid_request" "missing Sec-WebSocket-Key")
  | Some key ->
    let headers =
      Http.Header.of_list
        [ "upgrade", "websocket"
        ; "connection", "Upgrade"
        ; ( "sec-websocket-accept"
          , Websocket.sec_websocket_accept_header_value ~sec_websocket_key:key )
        ]
    in
    (* [Http.Response.make], unlike [Cohttp.Response.make], does not add
       [transfer-encoding: chunked], which is invalid on a 101. *)
    Ok
      ( Http.Response.make ~status:`Switching_protocols ~headers ()
      , fun reader writer ->
          let ws = Websocket.create ~opcode:`Text ~role:Server reader writer in
          let incoming, outgoing = Websocket.pipes ws in
          let pushes = Hub.subscribe t.ctx.hub s.customer in
          let close () = Pipe.close_read pushes in
          t.ws_closers <- close :: t.ws_closers;
          don't_wait_for
            (Pipe.iter_without_pushback pushes ~f:(fun push ->
               if not (Pipe.is_closed outgoing)
               then Pipe.write_without_pushback outgoing (A.Ws_push.to_string push)));
          (* Nothing is expected from the client; reading keeps control frames flowing and
             notices the close. *)
          let%bind () = Pipe.drain incoming in
          Pipe.close outgoing;
          close ();
          return () )
;;

(* Serving *)

let cors_headers t (r : request) =
  match header r "origin" with
  | None -> []
  | Some origin ->
    let allow =
      if List.mem t.config.allowed_origins "*" ~equal:String.equal
      then Some "*"
      else if List.mem t.config.allowed_origins origin ~equal:String.equal
      then Some origin
      else None
    in
    (match allow with
     | None -> []
     | Some allow ->
       [ "access-control-allow-origin", allow
       ; "vary", "Origin"
       ; "access-control-allow-methods", "GET, POST, PUT, DELETE, OPTIONS"
       ; "access-control-allow-headers", "authorization, content-type, idempotency-key"
       ; "access-control-max-age", "600"
       ])
;;

let read_body t ~body (req : Cohttp.Request.t) =
  let too_large =
    match Cohttp.Header.get (Cohttp.Request.headers req) "content-length" with
    | Some n ->
      Option.value_map (Int.of_string_opt n) ~default:false ~f:(fun n ->
        n > t.config.max_body)
    | None -> false
  in
  if too_large
  then return None
  else (
    let%map s = Cohttp_async.Body.to_string body in
    if String.length s > t.config.max_body then None else Some s)
;;

let handler t ~body _addr (req : Cohttp.Request.t) =
  let started = Time_ns.now () in
  let meth = Cohttp.Request.meth req in
  let uri = Cohttp.Request.uri req in
  let path =
    String.split (Uri.path uri) ~on:'/' |> List.filter ~f:(Fn.non String.is_empty)
  in
  let r0 = { meth; path; uri; headers = Cohttp.Request.headers req; body = "" } in
  let cors = cors_headers t r0 in
  let respond route_name (reply : Reply.t) =
    let status = Cohttp.Code.code_of_status reply.status in
    let duration = Time_ns.diff (Time_ns.now ()) started in
    let meth = Cohttp.Code.string_of_method meth in
    Metrics.request t.ctx.metrics ~route:route_name ~meth ~status ~duration;
    Log.Global.info_s
      [%message
        "request"
          ~method_:meth
          ~route:route_name
          (status : int)
          ~ms:(Time_ns.Span.to_ms duration : float)];
    let headers =
      Cohttp.Header.of_list
        ((("content-type", reply.content_type) :: reply.headers) @ cors)
    in
    let%map response =
      Cohttp_async.Server.respond_string ~headers ~status:reply.status reply.body
    in
    `Response response
  in
  match meth, path with
  | `OPTIONS, _ ->
    let%map response =
      Cohttp_async.Server.respond_string
        ~headers:(Cohttp.Header.of_list cors)
        ~status:`No_content
        ""
    in
    `Response response
  | `GET, [ "ws" ] when header_has r0 "upgrade" "websocket" ->
    (match Uri.get_query_param uri "token" with
     | None ->
       respond
         "/ws"
         (Reply.problem (Problem.unauthorized "no_session" "missing session token"))
     | Some token ->
       (match%bind Auth.authenticate t.ctx ~token with
        | Error p -> respond "/ws" (Reply.problem p)
        | Ok s ->
          (match ws_upgrade t r0 s with
           | Error p -> respond "/ws" (Reply.problem p)
           | Ok (response, handle) ->
             Metrics.request
               t.ctx.metrics
               ~route:"/ws"
               ~meth:"GET"
               ~status:101
               ~duration:(Time_ns.diff (Time_ns.now ()) started);
             return (`Expert (response, handle)))))
  | _ ->
    (match%bind read_body t ~body req with
     | None ->
       respond
         "body"
         (Reply.problem
            (Problem.create
               `Request_entity_too_large
               "too_large"
               "the request body is too large"))
     | Some body ->
       let r = { r0 with body } in
       let rt = route t r in
       let%bind reply =
         match%bind
           Monitor.try_with ~extract_exn:true ~rest:`Log (fun () ->
             match%bind authorize t r rt.access with
             | Error p -> return (Reply.problem p)
             | Ok who -> with_idempotency t r rt ~who)
         with
         | Ok reply -> return reply
         | Error exn ->
           Log.Global.error_s [%message "handler failed" (exn : exn)];
           return (Reply.problem (Problem.internal (Error.of_exn exn)))
       in
       respond rt.name reply)
;;

let start t ~port =
  Metrics.gauge t.ctx.metrics "ws_connections" (fun () ->
    Float.of_int (Hub.connections t.ctx.hub));
  Cohttp_async.Server.create_expert
    ~on_handler_error:
      (`Call (fun _ exn -> Log.Global.error_s [%message "handler error" (exn : exn)]))
    (Tcp.Where_to_listen.of_port port)
    (fun ~body addr req -> handler t ~body addr req)
;;

let close t =
  List.iter t.ws_closers ~f:(fun f -> f ());
  t.ws_closers <- [];
  Hub.close_all t.ctx.hub
;;
