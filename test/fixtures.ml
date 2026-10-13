(* Values for store and service tests. Every person and number here is synthetic. *)

open! Core
open Neobank_api
module Customer_id = Kyc_core.Customer_id

let start = Time_ns.of_string_with_utc_offset "2026-10-14 09:00:00Z"
let at_min m = Time_ns.add start (Time_ns.Span.of_min (Float.of_int m))
let eur s = Money.Amount.of_decimal_string Money.Currency.eur s |> ok_exn
let customer_id s = Customer_id.of_string s

let person =
  let open Kyc_core in
  { Person.given_name = Pii.create "Test"
  ; family_name = Pii.create "Customer"
  ; date_of_birth = Pii.create (Date.of_string "1990-01-01")
  ; nationality = Pii.create (Country.of_string "DE")
  ; residential_address =
      Pii.create
        { Address.line1 = "1 Test Street"
        ; line2 = None
        ; city = "Berlin"
        ; region = None
        ; postal_code = Some "10115"
        ; country = Country.of_string "DE"
        }
  ; email = Pii.create "customer@example.com"
  ; mobile = Pii.create "+4930000000"
  }
;;

let customer ?(email = "customer@example.com") ?provider_ref id : Model.Customer.t =
  { id = customer_id id
  ; email = Model.Customer.normalize_email email
  ; password_hash = Model.Secret.of_string "pbkdf2-sha256$1$00$00"
  ; totp_secret = Model.Secret.of_string "12345678901234567890"
  ; totp_confirmed = false
  ; last_totp_step = None
  ; provider_ref = Option.map provider_ref ~f:Banking_provider.Customer_ref.of_string
  ; sca_lock = Payments_core.Sca.Lock.empty
  ; failed_sca_at = []
  ; created_at = start
  }
;;

let account ?(currency = Money.Currency.eur) ~customer id : Model.Account.t =
  { customer = customer_id customer
  ; provider =
      { id = Banking_provider.Account_ref.of_string id
      ; customer = Banking_provider.Customer_ref.of_string ("ref-" ^ customer)
      ; currency
      ; status = Ledger.Account_status.Open
      ; addresses = []
      ; opened_at = start
      }
  ; updated_at = start
  }
;;

(* The example creditor IBAN printed in public IBAN references. *)
let payee_iban = "FR1420041010050500013M02606"
let debtor_iban = "DE89370400440532013000"

let beneficiary ~owner id =
  Payments_core.Beneficiary.create
    ~id:(Payments_core.Beneficiary.Id.of_string id)
    ~owner
    ~name:"Payee"
    ~rail:Sepa_credit
    ~now:start
    payee_iban
  |> ok_exn
;;

let draft ?(amount = eur "30.00") ~owner id =
  let open Payments_core in
  let party name iban = { Iso20022.Party.name; iban; bic = None; address = None } in
  Transfer.draft
    ~id:(Transfer.Id.of_string id)
    ~rail:Sepa_credit
    ~account:(Ledger.Account_id.of_string ("app:" ^ owner))
    ~beneficiary:(beneficiary ~owner ("ben-" ^ id))
    ~amount
    ~details:
      (Sepa
         (Sepa.Details.create
            ~payment_id:(Iso20022.Payment_id.create ~end_to_end_id:id () |> ok_exn)
            ~debtor:(party "Test Customer" (Iban.of_string_exn debtor_iban))
            ~creditor:(party "Payee" (Iban.of_string_exn payee_iban))
            ()))
    ~now:start
    ()
  |> ok_exn
;;

let transfer ?(created_at = start) ~customer ~account id : Model.Transfer.t =
  { transfer = draft ~owner:customer id
  ; customer = customer_id customer
  ; account = Banking_provider.Account_ref.of_string account
  ; beneficiary = Some (Payments_core.Beneficiary.Id.of_string ("ben-" ^ id))
  ; risk = None
  ; hold = Not_held
  ; challenge = None
  ; provider_challenge = None
  ; created_at
  ; updated_at = created_at
  }
;;

let card ~customer ~request_id id : Model.Card.t =
  { id = Card_core.Ids.Card_id.of_string id
  ; customer = customer_id customer
  ; account = Banking_provider.Account_ref.of_string "acc-eur"
  ; request_id
  ; kind = Virtual
  ; status = Active
  ; masked_pan = None
  ; limits = Card_core.Limits.unlimited Money.Currency.eur
  ; created_at = start
  ; updated_at = start
  }
;;

let event ?(kind = Banking_provider.Event.Kind.Ping) id : Banking_provider.Event.t =
  { id = Banking_provider.Event_id.of_string id; at = start; kind }
;;

let audit action : Model.Audit.Record.t =
  { actor = "customer:c1"
  ; on_behalf_of = None
  ; action
  ; object_id = "t1"
  ; at = start
  ; source = "test"
  ; outcome = "ok"
  }
;;
