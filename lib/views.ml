open! Core
open Neobank_api_types
open Model

let time t =
  let date, ofday = Time_ns.to_date_ofday t ~zone:Time_float.Zone.utc in
  let parts = Time_ns.Ofday.to_parts ofday in
  sprintf
    "%sT%02d:%02d:%02d.%03dZ"
    (Date.to_string date)
    parts.hr
    parts.min
    parts.sec
    parts.ms
;;

let amount a =
  { Api_types.Amount.amount = Money.Amount.to_decimal_string a
  ; currency = Money.Currency.to_string (Money.Amount.currency a)
  }
;;

let kind_name k = String.lowercase (Sexp.to_string [%sexp (k : Payments_core.Sca.Kind.t)])

let sca_challenge (c : Payments_core.Sca.Challenge.t) =
  { Api_types.Sca_info.challenge_id = Payments_core.Sca.Id.to_string c.id
  ; factors = List.map c.factors ~f:kind_name
  ; expires_at = time c.expires_at
  ; attempts_left = c.max_attempts - c.attempts
  }
;;

let provider_challenge (c : Provider_challenge.t) =
  { Api_types.Sca_info.challenge_id = Payments_core.Sca.Id.to_string c.id
  ; factors = List.map c.factors ~f:kind_name
  ; expires_at = time c.expires_at
  ; attempts_left = c.attempts_left
  }
;;

let lower_sexp sexp = String.lowercase (Sexp.to_string sexp)

let kyc (v : Kyc_core.Kyc_case.Customer_view.t) =
  { Api_types.Kyc.Status.status =
      lower_sexp [%sexp (v.status : Kyc_core.Kyc_case.Customer_view.status)]
  ; tier = Sexp.to_string [%sexp (v.tier : Kyc_core.Tier.t)]
  ; steps_to_complete =
      List.map v.steps_to_complete ~f:(fun s ->
        lower_sexp [%sexp (s : Kyc_core.Verification.Step.t)])
  ; fields_to_resubmit =
      List.map v.fields_to_resubmit ~f:(fun f ->
        lower_sexp [%sexp (f : Kyc_core.Person.Field.t)])
  }
;;

let balance (b : Banking_provider.Account.Balance.t) =
  { Api_types.Balance.available = amount b.available
  ; held = amount b.held
  ; posted = amount b.posted
  ; as_of = time b.as_of
  }
;;

let account ?balance:b (a : Account.t) =
  let find f = List.find_map a.provider.addresses ~f in
  { Api_types.Account.id = Account_ref.to_string (Account.id a)
  ; currency = Money.Currency.to_string (Account.currency a)
  ; status = lower_sexp [%sexp (a.provider.status : Ledger.Account_status.t)]
  ; iban =
      find (function
        | Iban { iban; _ } -> Some (Payments_core.Iban.to_string iban)
        | _ -> None)
  ; bic =
      find (function
        | Iban { bic = Some bic; _ } -> Some (Payments_core.Bic.to_string bic)
        | _ -> None)
  ; address =
      find (function
        | Crypto { address; _ } -> Some (Payments_core.Crypto_address.address address)
        | _ -> None)
  ; network =
      find (function
        | Crypto { address; _ } ->
          Some
            (Payments_core.Network.to_string
               (Payments_core.Crypto_address.network address))
        | _ -> None)
  ; lightning_node =
      find (function
        | Lightning_node { node_id } -> Some node_id
        | _ -> None)
  ; balance = Option.map b ~f:balance
  }
;;

let statement (s : Banking_provider.Statement.t) =
  { Api_types.Statement.account_id = Account_ref.to_string s.account
  ; currency = Money.Currency.to_string s.currency
  ; from = time s.from
  ; until = time s.until
  ; opening = amount s.opening
  ; closing = amount s.closing
  ; lines =
      List.map s.lines ~f:(fun (l : Banking_provider.Statement.Line.t) ->
        { Api_types.Statement.Line.at = time l.at
        ; amount = amount l.amount
        ; code = l.code
        ; reference = l.reference
        ; balance = amount l.balance
        })
  }
;;

let rail_to_string : Payments_core.Rail.t -> string = function
  | Sepa_credit -> "sepa"
  | Sepa_instant -> "sepa_instant"
  | Onchain n -> "onchain:" ^ String.lowercase (Payments_core.Network.to_string n)
  | Lightning -> "lightning"
  | Intraledger -> "intraledger"
  | Card_scheme -> "card"
;;

let rail_of_string s : Payments_core.Rail.t Or_error.t =
  match String.lowercase (String.strip s) with
  | "sepa" | "sepa_credit" -> Ok Sepa_credit
  | "sepa_instant" -> Ok Sepa_instant
  | "lightning" -> Ok Lightning
  | "intraledger" -> Ok Intraledger
  | s ->
    (match String.chop_prefix s ~prefix:"onchain:" with
     | Some network ->
       (match
          List.find Payments_core.Network.all ~f:(fun n ->
            String.equal (String.lowercase (Payments_core.Network.to_string n)) network)
        with
        | Some n -> Ok (Onchain n)
        | None -> Or_error.error_s [%message "unknown network" network])
     | None -> Or_error.error_s [%message "unknown rail" s])
;;

let destination_text : Payments_core.Beneficiary.Destination.t -> string = function
  | Bank_account { iban; _ } -> Redact.iban (Payments_core.Iban.to_string iban)
  | Crypto { address; _ } ->
    Redact.crypto_address (Payments_core.Crypto_address.address address)
  | Lightning target ->
    (match target with
     | Address { user; domain } -> Redact.email (user ^ "@" ^ domain)
     | Invoice _ -> "lightning invoice"
     | Lnurl _ -> "lnurl")
  | Internal { account } -> account
;;

let beneficiary (b : Payments_core.Beneficiary.t) =
  { Api_types.Beneficiary.id = Payments_core.Beneficiary.Id.to_string b.id
  ; name = b.name
  ; rails =
      List.map
        (Payments_core.Beneficiary.Destination.rails b.destination)
        ~f:rail_to_string
  ; destination = destination_text b.destination
  ; trusted = b.trusted
  ; payee_check =
      Option.map b.payee_check ~f:(fun c ->
        match c with
        | Match -> "match"
        | Close_match _ -> "close_match"
        | No_match -> "no_match"
        | Not_possible -> "not_possible")
  ; created_at = time b.first_seen
  }
;;

let fee_quote (q : Banking_provider.Transfer_info.Fee_quote.t) =
  { Api_types.Fee_quote.rail = rail_to_string q.rail
  ; amount = amount q.amount
  ; fee = amount q.fee
  ; total = amount q.total
  }
;;

let transfer_status (t : Transfer.t) =
  match t.hold with
  | Under_review _ -> "under_review"
  | Not_held | Released _ | Refused _ ->
    String.lowercase (Payments_core.Transfer.Status.name t.transfer.status)
;;

let counterparty_text : Payments_core.Transfer.Counterparty.t -> string = function
  | Beneficiary b -> b.name
  | External { name; _ } -> name
;;

let transfer (t : Transfer.t) =
  let x = t.transfer in
  let open_challenge =
    Option.filter t.challenge ~f:(fun c ->
      match c.status with
      | Pending -> true
      | Verified _ | Failed _ -> false)
  in
  { Api_types.Transfer.id = Payments_core.Transfer.Id.to_string x.id
  ; account_id = Account_ref.to_string t.account
  ; beneficiary_id = Option.map t.beneficiary ~f:Payments_core.Beneficiary.Id.to_string
  ; counterparty = counterparty_text x.counterparty
  ; direction =
      (match x.direction with
       | In -> "in"
       | Out -> "out")
  ; rail = rail_to_string x.rail
  ; amount = amount x.amount
  ; fee = amount x.fee
  ; reference = x.reference
  ; status = transfer_status t
  ; under_review =
      (match t.hold with
       | Under_review _ -> true
       | _ -> false)
  ; sca = Option.map open_challenge ~f:sca_challenge
  ; provider_sca = Option.map t.provider_challenge ~f:provider_challenge
  ; message =
      (match t.hold, x.status with
       | Under_review _, _ -> Some "under review"
       | Refused _, _ -> Some "this payment could not be made"
       | _, (Rejected _ | Returned _ | Failed _) -> Some "the payment did not go through"
       | _ -> None)
  ; created_at = time t.created_at
  ; updated_at = time t.updated_at
  }
;;

let swap (s : Swap.t) =
  { Api_types.Swap.id = Payments_core.Swap.Id.to_string s.id
  ; from_account_id = Account_ref.to_string s.from_account
  ; to_account_id = Account_ref.to_string s.to_account
  ; sell = amount s.sell
  ; fee = amount s.fee
  ; buy = amount s.buy
  ; price = s.price
  ; expires_at = time s.expires_at
  ; status =
      (match s.status with
       | Quoted -> "quoted"
       | Executing -> "executing"
       | Awaiting_sca -> "awaiting_sca"
       | Executed -> "executed"
       | Failed _ -> "failed")
  ; provider_sca = Option.map s.provider_challenge ~f:provider_challenge
  }
;;

let card (c : Card.t) =
  let l = c.limits in
  { Api_types.Card.id = Card_core.Ids.Card_id.to_string c.id
  ; account_id = Account_ref.to_string c.account
  ; kind = lower_sexp [%sexp (c.kind : Card_core.Card.Kind.t)]
  ; status = lower_sexp [%sexp (c.status : Card_core.Card.Status.t)]
  ; masked_pan = Option.map c.masked_pan ~f:Card_core.Masked_pan.to_string
  ; limits =
      { per_transaction = Option.map l.per_transaction ~f:amount
      ; daily = Option.map l.daily ~f:amount
      ; monthly = Option.map l.monthly ~f:amount
      ; online = l.online
      ; contactless = l.contactless
      ; atm = l.atm
      }
  ; created_at = time c.created_at
  }
;;

let schedule_to_string : Payments_core.Schedule.t -> string = function
  | Daily -> "daily"
  | Weekly day -> "weekly:" ^ String.lowercase (Day_of_week.to_string day)
  | Monthly { day } -> sprintf "monthly:%d" day
;;

let schedule_of_string s : Payments_core.Schedule.t Or_error.t =
  let open Or_error.Let_syntax in
  let%bind schedule =
    match String.lsplit2 (String.lowercase (String.strip s)) ~on:':' with
    | None when String.equal (String.lowercase (String.strip s)) "daily" ->
      Ok Payments_core.Schedule.Daily
    | Some ("weekly", day) ->
      Or_error.try_with (fun () ->
        Payments_core.Schedule.Weekly (Day_of_week.of_string day))
    | Some ("monthly", day) ->
      (match Int.of_string_opt day with
       | Some day -> Ok (Payments_core.Schedule.Monthly { day })
       | None -> Or_error.error_s [%message "monthly day is not a number" day])
    | _ ->
      Or_error.error_s [%message "unknown schedule (daily, weekly:mon, monthly:15)" s]
  in
  let%map () = Payments_core.Schedule.validate schedule in
  schedule
;;

let standing_order (o : Standing_order.t) =
  let x = o.order in
  { Api_types.Standing_order.id = Payments_core.Standing_order.Id.to_string x.id
  ; account_id = Account_ref.to_string o.account
  ; beneficiary_id = Payments_core.Beneficiary.Id.to_string x.beneficiary.id
  ; amount = amount x.amount
  ; schedule = schedule_to_string x.schedule
  ; next_run = Date.to_string x.next_run
  ; status =
      (match o.provider_ref, o.provider_challenge, x.status with
       | _, Some _, Active -> "awaiting_sca"
       | None, None, Active -> "pending"
       | _, _, status ->
         lower_sexp [%sexp (status : Payments_core.Standing_order.Status.t)])
  ; runs = x.runs
  ; provider_sca = Option.map o.provider_challenge ~f:provider_challenge
  }
;;

let review (r : Review.t) =
  { Api_types.Review.id = r.id
  ; reference = r.reference
  ; customer_id = Customer_id.to_string r.customer
  ; severity = String.lowercase (Risk_rules.Severity.to_string r.risk.decision.severity)
  ; reasons =
      List.map r.risk.decision.hits ~f:(fun (h : Risk_rules.Hit.t) ->
        sprintf "%s: %s" (Risk_rules.Reason_code.to_string h.code) h.reason)
  ; submitted_at = time r.submitted_at
  ; status =
      (match r.status with
       | Pending -> "pending"
       | Approved _ -> "approved"
       | Rejected _ -> "rejected")
  ; operator =
      (match r.status with
       | Pending -> None
       | Approved { operator; _ } | Rejected { operator; _ } -> Some operator)
  }
;;
