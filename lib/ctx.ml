open! Core
open Async
open Model

module Config = struct
  type t =
    { password_iterations : int
    ; session_idle : Time_ns.Span.t
    ; session_max : Time_ns.Span.t
    ; sca_fresh_for : Time_ns.Span.t
    ; challenge_ttl : Time_ns.Span.t
    ; totp_issuer : string
    ; currencies : Money.Currency.t list
    ; indicative_eur : (Money.Currency.t * string) list
    ; relay_interval : Time_ns.Span.t
    ; relay_backoff_max : Time_ns.Span.t
    ; relay_give_up_after : Time_ns.Span.t
    ; idempotency_ttl : Time_ns.Span.t
    ; inbox_ttl : Time_ns.Span.t
    ; ops_token : string option
    ; kyc_policy : Kyc_core.Kyc_case.Policy.t
    }

  let demo_kyc_policy =
    { Kyc_core.Kyc_case.Policy.default with
      require_screening = false
    ; require_risk_assessment = false
    }
  ;;

  let default =
    { password_iterations = Password.default_iterations
    ; session_idle = Payments_core.Sca.Session.idle_timeout
    ; session_max = Time_ns.Span.of_hr 12.
    ; sca_fresh_for = Time_ns.Span.of_min 5.
    ; challenge_ttl = Time_ns.Span.of_min 5.
    ; totp_issuer = "Neobank Demo"
    ; currencies = Money.Currency.[ eur; btc; btc_lightning; eth; usdc; usdt ]
    ; indicative_eur =
        Money.Currency.
          [ eur, "1"
          ; btc, "60000"
          ; btc_lightning, "60000"
          ; eth, "3000"
          ; usdc, "1"
          ; usdt, "1"
          ]
    ; relay_interval = Time_ns.Span.of_sec 1.
    ; relay_backoff_max = Time_ns.Span.of_min 5.
    ; relay_give_up_after = Time_ns.Span.of_day 1.
    ; idempotency_ttl = Time_ns.Span.of_day 1.
    ; inbox_ttl = Time_ns.Span.of_day 30.
    ; ops_token = None
    ; kyc_policy = demo_kyc_policy
    }
  ;;
end

type t =
  { config : Config.t
  ; store : Any_store.t
  ; bank : Bank.t
  ; time_source : Time_source.t
  ; ids : Ids.t
  ; policy : Risk_rules.Policy.t
  ; metrics : Metrics.t
  ; hub : Hub.t
  ; locks : Keyed_lock.t
  ; mutable kick : unit -> unit Deferred.t
  }

let create
  ?(ids = Ids.random ())
  ?(time_source = Time_source.wall_clock ())
  ?(policy = Risk_policy.default)
  ~config
  ~store
  ~bank
  ()
  =
  { config
  ; store
  ; bank
  ; time_source
  ; ids
  ; policy
  ; metrics = Metrics.create ()
  ; hub = Hub.create ()
  ; locks = Keyed_lock.create ()
  ; kick = (fun () -> return ())
  }
;;

let now t = Time_source.now t.time_source
let read d = Deferred.map d ~f:Problem.of_internal

let find ~what d =
  Deferred.map d ~f:(function
    | Ok (Some x) -> Ok x
    | Ok None -> Error (Problem.not_found what)
    | Error e -> Error (Problem.internal e))
;;

let commit t writes =
  match%map Any_store.commit t.store writes with
  | Ok (Committed | Duplicate_event) -> Ok ()
  | Ok (Conflict code) ->
    Error (Problem.conflict code (String.tr code ~target:'_' ~replacement:' '))
  | Error e ->
    Log.Global.error_s [%message "store commit failed" (e : Error.t)];
    Error (Problem.internal e)
;;

let audit t ~actor ?on_behalf_of ~action ~object_id ~outcome () =
  Store.Write.Audit
    { Audit.Record.actor
    ; on_behalf_of
    ; action
    ; object_id
    ; at = now t
    ; source = "neobank-api"
    ; outcome
    }
;;

let eur_value t amount =
  let currency = Money.Amount.currency amount in
  List.Assoc.find t.config.indicative_eur currency ~equal:Money.Currency.equal
  |> Option.bind ~f:(fun price ->
    Option.bind
      (Result.ok (Money.Rate.price_of_string price))
      ~f:(fun price ->
        if Money.Currency.equal currency Money.Currency.eur
        then Some amount
        else
          Some
            (Money.Amount.of_q
               Money.Currency.eur
               (Q.mul (Money.Amount.to_q amount) price))))
;;
