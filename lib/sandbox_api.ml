open! Core
open Async
open Neobank_api_types
open Model

let ( let* ) = Deferred.Result.( >>= )
let ok = Deferred.Result.return
let fail p = return (Error p)
let validation r = return (Problem.of_validation r)
let provider r = Deferred.map r ~f:Problem.of_provider

let require_sandbox (ctx : Ctx.t) =
  if Bank.has_sandbox ctx.bank
  then ok ()
  else fail (Problem.not_found "route (the provider has no sandbox)")
;;

let kyc_decision (ctx : Ctx.t) customer (r : Api_types.Sandbox.Kyc_decision.t) =
  let* () = require_sandbox ctx in
  let* c = Ctx.find ~what:"customer" (Any_store.customer ctx.store customer) in
  let* provider_ref =
    match c.provider_ref with
    | Some p -> ok p
    | None ->
      fail (Problem.conflict "provider_pending" "the account is still being set up")
  in
  let* decision =
    match String.lowercase r.decision with
    | "approve" -> ok Banking_provider.Sandbox.Kyc_decision.Approve
    | "reject" ->
      ok
        (Banking_provider.Sandbox.Kyc_decision.Reject
           (Option.value r.reason ~default:"rejected in the sandbox"))
    | "resubmit" ->
      ok
        (Banking_provider.Sandbox.Kyc_decision.Request_resubmission
           [ Kyc_core.Person.Field.Residential_address ])
    | _ ->
      fail
        (Problem.bad_request "invalid_request" "decision is approve, reject or resubmit")
  in
  let* () = provider (Bank.simulate_kyc_decision ctx.bank provider_ref decision) in
  (* The decision arrives as events; let them land before answering. *)
  let%bind () = ctx.kick () in
  Onboarding.status ctx customer
;;

let default_rail currency : Payments_core.Rail.t =
  match Money.Currency.code currency, Money.Currency.network currency with
  | "EUR", _ -> Sepa_instant
  | "BTC", Some "lightning" -> Lightning
  | "BTC", _ -> Onchain Bitcoin
  | _ -> Onchain Ethereum
;;

let deposit (ctx : Ctx.t) customer (r : Api_types.Sandbox.Deposit.t) =
  let* () = require_sandbox ctx in
  let* account = Accounts.owned ctx customer r.account_id in
  let* amount = validation (Payments.parse_amount r.amount) in
  let* rail =
    match r.rail with
    | Some s -> validation (Views.rail_of_string s)
    | None -> ok (default_rail (Account.currency account))
  in
  let* id =
    provider (Bank.simulate_deposit ctx.bank ~account:(Account.id account) ~rail amount)
  in
  let%bind () = ctx.kick () in
  ok (Payments_core.Transfer.Id.to_string id)
;;

let card_auth (ctx : Ctx.t) customer (r : Api_types.Sandbox.Card_auth.t) =
  let* () = require_sandbox ctx in
  let* card =
    Ctx.find
      ~what:"card"
      (Any_store.card ctx.store (Card_core.Ids.Card_id.of_string r.card_id))
  in
  let* () =
    if Customer_id.equal card.customer customer
    then ok ()
    else fail (Problem.not_found "card")
  in
  let* merchant =
    validation
      (let open Or_error.Let_syntax in
       let%bind mcc = Card_core.Mcc.of_int r.mcc in
       let%bind country =
         Card_core.Country.of_string_or_error (String.uppercase r.country)
       in
       let%bind amount = Payments.parse_amount r.amount in
       Card_core.Merchant.create ~name:r.merchant ~mcc ~country ~amount)
  in
  let* code = provider (Bank.simulate_card_auth ctx.bank card.id merchant) in
  ok
    { Api_types.Sandbox.Card_auth_result.response_code =
        Card_core.Response_code.to_string code
    ; approved = Card_core.Response_code.is_approved code
    }
;;

let transfer_outcome (ctx : Ctx.t) customer (r : Api_types.Sandbox.Transfer_outcome.t) =
  let* () = require_sandbox ctx in
  let* (_ : Api_types.Transfer.t) = Payments.get_transfer ctx customer r.transfer_id in
  let reason () =
    Payments_core.Iso_reason.of_code (Option.value r.reason ~default:"AC04")
  in
  let* update =
    validation
      (match String.lowercase r.outcome with
       | "settled" -> Ok Banking_provider.Transfer_update.Settled
       | "rejected" ->
         Or_error.map (reason ()) ~f:(fun r ->
           Banking_provider.Transfer_update.Rejected r)
       | "returned" ->
         Or_error.map (reason ()) ~f:(fun r ->
           Banking_provider.Transfer_update.Returned r)
       | _ -> Or_error.error_string "outcome is settled, rejected or returned")
  in
  let* () =
    provider
      (Bank.simulate_transfer_outcome
         ctx.bank
         (Payments_core.Transfer.Id.of_string r.transfer_id)
         update)
  in
  let%bind () = ctx.kick () in
  Payments.get_transfer ctx customer r.transfer_id
;;
