(** Accounts and statements. Balances and statements come from the provider, which holds
    the money; our store knows which accounts belong to whom. Viewing them needs a live
    session only (balance and 90-day history are SCA-exempt, RTS art. 10). *)

open! Core
open Neobank_api_types
open Model

(** The customer's accounts with their balances (an account whose balance the provider
    cannot give right now is listed without one). *)
val list : Ctx.t -> Customer_id.t -> Api_types.Account.t list Problem.res

(** 404 unless the account belongs to the customer. *)
val owned : Ctx.t -> Customer_id.t -> string -> Account.t Problem.res

val get : Ctx.t -> Customer_id.t -> string -> Api_types.Account.t Problem.res

(** [from] and [until] default to the last 30 days; at most 90 days back without a fresh
    SCA. *)
val statement
  :  Ctx.t
  -> Session.t
  -> string
  -> from:Time_ns.t option
  -> until:Time_ns.t option
  -> Banking_provider.Statement.t Problem.res

(** One header line and one line per movement:
    [at,amount,currency,code,reference,balance]. *)
val statement_csv : Banking_provider.Statement.t -> string
