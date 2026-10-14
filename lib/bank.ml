open! Core
open Async
open Banking_provider

module Card_ui = struct
  type display =
    { provider : string
    ; script_url : string option
    ; application_id : string option
    ; ui_secret : Sensitive.t option
    ; user_id : string option
    ; card_id : string
    }
  [@@deriving sexp_of]

  type t =
    { display : Card_core.Ids.Card_id.t -> display Or_error.t Deferred.t
    ; consent_started :
        Card_core.Ids.Card_id.t
        -> challenge_id:string
        -> Sca_challenge.t Or_error.t Deferred.t
    }
end

type t =
  | T :
      { impl : (module Provider.S with type t = 'a)
      ; sandbox : (module Sandbox.S with type t = 'a) option
      ; provider : 'a
      ; fake : Fake_provider.t option
      ; journal : Ledger.Journal.t option
      ; card_ui : Card_ui.t option
      ; checkpoint : unit -> unit
      ; on_close : unit -> unit
      }
      -> t

let of_provider impl ?sandbox ?card_ui provider =
  T
    { impl
    ; sandbox
    ; provider
    ; fake = None
    ; journal = None
    ; card_ui
    ; checkpoint = Fn.id
    ; on_close = Fn.id
    }
;;

(* The fake's id counters, saved next to its journal. A restored counter starts
   [counter_margin] past the saved value: saves happen after events and on every relay
   pass, so ids handed out after the last save are skipped, never reissued. *)
let counters_file state_dir = state_dir ^/ "fake-counters.sexp"
let counter_margin = 1000

let load_counters state_dir =
  let path = counters_file state_dir in
  match Sys_unix.file_exists path with
  | `No | `Unknown -> Ok []
  | `Yes ->
    Or_error.try_with (fun () ->
      Core.Sexp.load_sexp_conv_exn path [%of_sexp: (string * int) list]
      |> List.map ~f:(fun (prefix, n) -> prefix, n + counter_margin))
    |> Or_error.tag ~tag:path
;;

let save_counters state_dir counters =
  let path = counters_file state_dir in
  let tmp = path ^ ".tmp" in
  (* A few hundred bytes, written synchronously so the save is complete when the call
     returns. *)
  Core.Sexp.save_hum tmp [%sexp (counters : (string * int) list)];
  Core_unix.rename ~src:tmp ~dst:path
;;

let fake ?config ~time_source ~state_dir () =
  let open Or_error.Let_syntax in
  let%bind () = Or_error.try_with (fun () -> Core_unix.mkdir_p state_dir) in
  let%bind counters = load_counters state_dir in
  let path = state_dir ^/ "journal" in
  let%bind journal =
    match Sys_unix.file_exists path with
    | `Yes -> Ledger.Journal.open_ ~time_source path
    | `No | `Unknown -> Ledger.Journal.create ~time_source path
  in
  let%map provider = Fake_provider.create ?config ~counters ~time_source ~journal () in
  let saved = ref counters in
  let checkpoint () =
    let now = Fake_provider.counters provider in
    if not ([%equal: (string * int) list] now !saved)
    then (
      save_counters state_dir now;
      saved := now)
  in
  checkpoint ();
  T
    { impl = (module Fake_provider)
    ; sandbox = Some (module Fake_provider)
    ; provider
    ; fake = Some provider
    ; journal = Some journal
    ; card_ui = None
    ; checkpoint
    ; on_close =
        (fun () ->
          checkpoint ();
          Ledger.Journal.close journal)
    }
;;

let as_fake (T t) = t.fake
let checkpoint (T t) = t.checkpoint ()
let journal (T t) = t.journal
let name (T { impl = (module P); provider; _ }) = P.name provider
let has_sandbox (T t) = Option.is_some t.sandbox
let close (T t) = t.on_close ()

let create_customer (T { impl = (module P); provider; _ }) ~customer_id ~person =
  P.create_customer provider ~customer_id ~person
;;

let start_verification (T { impl = (module P); provider; _ }) customer step =
  P.start_verification provider customer step
;;

let complete_verification (T { impl = (module P); provider; _ }) customer step ~code =
  P.complete_verification provider customer step ~code
;;

let kyc_status (T { impl = (module P); provider; _ }) c = P.kyc_status provider c

let open_account (T { impl = (module P); provider; _ }) c currency =
  P.open_account provider c currency
;;

let accounts (T { impl = (module P); provider; _ }) c = P.accounts provider c
let balance (T { impl = (module P); provider; _ }) a = P.balance provider a
let enrich_account (T { impl = (module P); provider; _ }) a = P.enrich_account provider a

let statement (T { impl = (module P); provider; _ }) a ~from ~until =
  P.statement provider a ~from ~until
;;

let quote_fee (T { impl = (module P); provider; _ }) ~from ~rail amount =
  P.quote_fee provider ~from ~rail amount
;;

let initiate_transfer (T { impl = (module P); provider; _ }) ~from transfer =
  P.initiate_transfer provider ~from transfer
;;

let transfer (T { impl = (module P); provider; _ }) id = P.transfer provider id

let confirm_sca (T { impl = (module P); provider; _ }) id ~code =
  P.confirm_sca provider id ~code
;;

let quote_swap (T { impl = (module P); provider; _ }) ~from ~to_ ~sell =
  P.quote_swap provider ~from ~to_ ~sell
;;

let execute_swap (T { impl = (module P); provider; _ }) id = P.execute_swap provider id

let issue_virtual_card (T { impl = (module P); provider; _ }) ~request_id c ~account =
  P.issue_virtual_card provider ~request_id c ~account
;;

let card (T { impl = (module P); provider; _ }) id = P.card provider id

let set_card_status (T { impl = (module P); provider; _ }) id status =
  P.set_card_status provider id status
;;

let set_card_limits (T { impl = (module P); provider; _ }) id limits =
  P.set_card_limits provider id limits
;;

let card_details_token (T { impl = (module P); provider; _ }) id =
  P.card_details_token provider id
;;

let create_standing_order (T { impl = (module P); provider; _ }) ~from order =
  P.create_standing_order provider ~from order
;;

let cancel_standing_order (T { impl = (module P); provider; _ }) r =
  P.cancel_standing_order provider r
;;

let events (T { impl = (module P); provider; _ }) = P.events provider
let ingest_webhook (T { impl = (module P); provider; _ }) r = P.ingest_webhook provider r

let no_sandbox (T { impl = (module P); provider; _ }) =
  Deferred.Or_error.error_s
    [%message "this provider has no sandbox" ~provider:(P.name provider : string)]
;;

let simulate_deposit (T t as bank) ~account ~rail amount =
  match t.sandbox with
  | Some (module S) -> S.simulate_deposit t.provider ~account ~rail amount
  | None -> no_sandbox bank
;;

let simulate_kyc_decision (T t as bank) customer decision =
  match t.sandbox with
  | Some (module S) -> S.simulate_kyc_decision t.provider customer decision
  | None -> no_sandbox bank
;;

let simulate_card_auth (T t as bank) card merchant =
  match t.sandbox with
  | Some (module S) -> S.simulate_card_auth t.provider card merchant
  | None -> no_sandbox bank
;;

let simulate_transfer_outcome (T t as bank) id update =
  match t.sandbox with
  | Some (module S) -> S.simulate_transfer_outcome t.provider id update
  | None -> no_sandbox bank
;;

let card_display (T t as bank) id =
  match t.card_ui with
  | Some ui -> ui.display id
  | None ->
    return
      (Ok
         { Card_ui.provider = name bank
         ; script_url = None
         ; application_id = None
         ; ui_secret = None
         ; user_id = None
         ; card_id = Card_core.Ids.Card_id.to_string id
         })
;;

let card_consent_started (T t as bank) id ~challenge_id =
  match t.card_ui with
  | Some ui -> ui.consent_started id ~challenge_id
  | None ->
    (match%map card_details_token bank id with
     | Error _ as e -> e
     | Ok (Challenge c) -> Ok c
     | Ok (Done _) ->
       Or_error.error_string "this card needs no consent; ask for the token")
;;
