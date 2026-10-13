open! Core
open Async
open Model

let ( let* ) = Deferred.Result.( >>= )
let ok = Deferred.Result.return
let fail p = return (Error p)

let list (ctx : Ctx.t) id =
  let* accounts = Ctx.read (Any_store.accounts ctx.store id) in
  let%map views =
    Deferred.List.map ~how:(`Max_concurrent_jobs 4) accounts ~f:(fun a ->
      match%map Bank.balance ctx.bank (Account.id a) with
      | Ok balance -> Views.account ~balance a
      | Error _ -> Views.account a)
  in
  Ok views
;;

let owned (ctx : Ctx.t) id account_id =
  let* account =
    Ctx.find
      ~what:"account"
      (Any_store.account ctx.store (Account_ref.of_string account_id))
  in
  if Customer_id.equal account.customer id
  then ok account
  else fail (Problem.not_found "account")
;;

let get ctx id account_id =
  let* account = owned ctx id account_id in
  match%map Bank.balance ctx.bank (Account.id account) with
  | Ok balance -> Ok (Views.account ~balance account)
  | Error _ -> Ok (Views.account account)
;;

let statement (ctx : Ctx.t) (s : Session.t) account_id ~from ~until =
  let* account = owned ctx s.customer account_id in
  let now = Ctx.now ctx in
  let until = Option.value until ~default:now in
  let from = Option.value from ~default:(Time_ns.sub until (Time_ns.Span.of_day 30.)) in
  let* () =
    if Time_ns.( >= ) from until
    then fail (Problem.bad_request "invalid_request" "from must be before until")
    else if Time_ns.Span.( > ) (Time_ns.diff now from) (Time_ns.Span.of_day 90.)
    then Auth.require_fresh_sca ctx s
    else ok ()
  in
  Deferred.map
    (Bank.statement ctx.bank (Account.id account) ~from ~until)
    ~f:Problem.of_provider
;;

let csv_field s =
  if String.exists s ~f:(fun c ->
       Char.equal c ',' || Char.equal c '"' || Char.equal c '\n')
  then "\"" ^ String.substr_replace_all s ~pattern:"\"" ~with_:"\"\"" ^ "\""
  else s
;;

let statement_csv (s : Banking_provider.Statement.t) =
  let line fields = String.concat ~sep:"," (List.map fields ~f:csv_field) in
  let rows =
    List.map s.lines ~f:(fun (l : Banking_provider.Statement.Line.t) ->
      line
        [ Views.time l.at
        ; Money.Amount.to_decimal_string l.amount
        ; Money.Currency.to_string (Money.Amount.currency l.amount)
        ; l.code
        ; l.reference
        ; Money.Amount.to_decimal_string l.balance
        ])
  in
  String.concat
    ~sep:"\n"
    (line [ "at"; "amount"; "currency"; "code"; "reference"; "balance" ] :: rows)
  ^ "\n"
;;
