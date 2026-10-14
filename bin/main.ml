open! Core
open! Async
open Neobank_api

let settlement =
  Command.Arg_type.create (fun s ->
    match String.lowercase s with
    | "immediate" -> Banking_provider.Fake_provider.Settlement.Immediate
    | "manual" -> Manual
    | s ->
      (match String.chop_prefix s ~prefix:"delayed:" with
       | Some secs -> Delayed (Time_ns.Span.of_sec (Float.of_string secs))
       | None ->
         failwithf "settlement is immediate, manual or delayed:<seconds>, not %s" s ()))
;;

let provider_kind = Command.Arg_type.of_alist_exn [ "fake", `Fake; "striga", `Striga ]

let store_kind =
  Command.Arg_type.of_alist_exn [ "memory", `Memory; "postgres", `Postgres ]
;;

let command =
  Command.async_or_error
    ~summary:"HTTP API and webhooks of the neobank demo"
    ~readme:(fun () ->
      String.concat
        ~sep:"\n"
        [ "State: PostgreSQL through DATABASE_URL (or the PG* variables) with -store \
           postgres, the default when DATABASE_URL is set; -store memory keeps \
           everything in the process."
        ; "Operators: NEOBANK_OPS_TOKEN enables /ops/* for that bearer token."
        ; "Provider: -provider fake runs offline with a simulated bank whose books are \
           the ledger journal in -state-dir; -provider striga reads STRIGA_* from the \
           environment."
        ])
    (let%map_open.Command port =
       flag "-port" (optional_with_default 8080 int) ~doc:"PORT listen port (8080)"
     and provider =
       flag
         "-provider"
         (optional_with_default `Fake provider_kind)
         ~doc:"fake|striga the banking provider (fake)"
     and store =
       flag
         "-store"
         (optional store_kind)
         ~doc:"memory|postgres where state lives (postgres when DATABASE_URL is set)"
     and database_url =
       flag "-database-url" (optional string) ~doc:"URL PostgreSQL (else DATABASE_URL)"
     and state_dir =
       flag
         "-state-dir"
         (optional_with_default "state" string)
         ~doc:"DIR the fake provider's ledger journal (./state)"
     and policy =
       flag "-policy" (optional string) ~doc:"FILE risk policy (sexp; built-in default)"
     and origins =
       flag
         "-allow-origin"
         (listed string)
         ~doc:"ORIGIN allow CORS from ORIGIN (repeatable; * for any)"
     and settlement =
       flag
         "-fake-settlement"
         (optional_with_default
            Banking_provider.Fake_provider.Settlement.Immediate
            settlement)
         ~doc:"MODE fake: immediate, manual or delayed:<seconds> (immediate)"
     and fake_sca =
       flag
         "-fake-sca"
         (optional_with_default true bool)
         ~doc:"BOOL fake: ask for the provider's own code (true)"
     and password_iterations =
       flag
         "-password-iterations"
         (optional_with_default Password.default_iterations int)
         ~doc:"N PBKDF2 rounds for new passwords (600000)"
     in
     fun () ->
       let open Deferred.Or_error.Let_syntax in
       Log.Global.set_output [ Log.Output.stderr () ];
       let%bind policy =
         match policy with
         | None -> return Risk_policy.default
         | Some file -> Deferred.return (Risk_rules.Policy.load file)
       in
       let%bind store =
         let use_pg =
           match store with
           | Some `Postgres -> true
           | Some `Memory -> false
           | None ->
             Option.is_some database_url || Option.is_some (Sys.getenv "DATABASE_URL")
         in
         if use_pg
         then (
           let%map pg = Store_pg.connect ?url:database_url () in
           Log.Global.info_s
             [%message
               "postgres"
                 ~target:(Store_pg.describe pg)
                 ~migrations_applied:(Store_pg.applied_migrations pg : int list)];
           Any_store.pack (module Store_pg) pg)
         else return (Any_store.pack (module Store_memory) (Store_memory.create ()))
       in
       let%bind bank =
         match provider with
         | `Fake ->
           Deferred.return
             (Bank.fake
                ~config:
                  { Banking_provider.Fake_provider.Config.default with
                    settlement
                  ; sca = fake_sca
                  ; auto_enrich = true
                  }
                ~time_source:(Time_source.wall_clock ())
                ~state_dir
                ())
         | `Striga ->
           Deferred.return
             (Neobank_api_striga.Striga_bank.create
                ~store
                ~now:(fun () -> Time_ns.now ())
                ())
       in
       let config =
         { Ctx.Config.default with
           password_iterations
         ; ops_token = Sys.getenv "NEOBANK_OPS_TOKEN"
         }
       in
       let ctx = Ctx.create ~policy ~config ~store ~bank () in
       let (_stop_events : unit -> unit) = Events.start ctx in
       Relay.start ctx ~stop:(Deferred.never ());
       let server =
         Server.create
           ~config:
             { Server.Config.default with
               allowed_origins =
                 (if List.is_empty origins
                  then Server.Config.default.allowed_origins
                  else origins)
             }
           ctx
       in
       let%bind.Deferred http = Server.start server ~port in
       Log.Global.info_s
         [%message
           "listening"
             ~port:(Cohttp_async.Server.listening_on http : int)
             ~store:(Any_store.kind_of ctx.store)
             ~provider:(Bank.name bank)];
       Deferred.ok (Cohttp_async.Server.close_finished http))
;;

let () = Command_unix.run command
