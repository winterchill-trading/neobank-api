open! Core

module Criterion = struct
  type t =
    | Clients
    | Reputation
    | Duration
    | Geography
    | Data_loss
    | Economic
  [@@deriving sexp, compare, equal, enumerate]
end

module Severity = struct
  type t =
    | Minor
    | Major
  [@@deriving sexp, compare, equal]
end

module Status = struct
  type t =
    | Open
    | Mitigated
    | Resolved
  [@@deriving sexp, compare, equal]
end

module Report = struct
  type kind =
    | Initial
    | Intermediate
    | Final
  [@@deriving sexp, compare, equal]

  type t =
    { kind : kind
    ; sent_at : Time_ns.Alternate_sexp.t
    }
  [@@deriving sexp, compare, equal]
end

type t =
  { id : string
  ; detected_at : Time_ns.Alternate_sexp.t
  ; classified_at : Time_ns.Alternate_sexp.t option
  ; critical_service_affected : bool
  ; services_affected : string list
  ; clients_affected : int
  ; clients_of_service : int
  ; duration : Time_ns.Span.t
  ; downtime : Time_ns.Span.t
  ; member_states : string list
  ; data_impact : bool
  ; reputational_impact : bool
  ; costs_eur : Money.Amount.t
  ; root_cause : string
  ; status : Status.t
  ; reports : Report.t list
  }
[@@deriving sexp, compare, equal]

let economic_threshold = Money.Amount.of_minor_units_int Money.Currency.eur 10_000_000

let criteria_met t =
  let clients =
    t.clients_affected > 100_000
    || (t.clients_of_service > 0 && t.clients_affected * 10 > t.clients_of_service)
  in
  let duration =
    Time_ns.Span.( > ) t.duration (Time_ns.Span.of_hr 24.)
    || (t.critical_service_affected
        && Time_ns.Span.( > ) t.downtime (Time_ns.Span.of_hr 2.))
  in
  let geography =
    List.length (List.dedup_and_sort t.member_states ~compare:String.Caseless.compare)
    >= 2
  in
  let economic =
    match Money.Amount.compare_value t.costs_eur economic_threshold with
    | Ok n -> n > 0
    | Error _ -> false
  in
  List.filter_map
    [ Criterion.Clients, clients
    ; Reputation, t.reputational_impact
    ; Duration, duration
    ; Geography, geography
    ; Data_loss, t.data_impact
    ; Economic, economic
    ]
    ~f:(fun (c, met) -> Option.some_if met c)
;;

let severity t : Severity.t =
  let met = criteria_met t in
  let others = List.count met ~f:(fun c -> not (Criterion.equal c Data_loss)) in
  if t.critical_service_affected
     && (List.mem met Data_loss ~equal:Criterion.equal || others >= 2)
  then Major
  else Minor
;;

module Deadlines = struct
  type t =
    { initial : Time_ns.Alternate_sexp.t
    ; intermediate : Time_ns.Alternate_sexp.t
    ; final : Time_ns.Alternate_sexp.t option
    }
  [@@deriving sexp, compare, equal]
end

let add_month time =
  let zone = Time_float.Zone.utc in
  let date, ofday = Time_ns.to_date_ofday time ~zone in
  Time_ns.of_date_ofday ~zone (Date.add_months date 1) ofday
;;

let last_sent t kind =
  List.filter t.reports ~f:(fun (r : Report.t) -> Report.equal_kind r.kind kind)
  |> List.map ~f:(fun r -> r.sent_at)
  |> List.max_elt ~compare:Time_ns.compare
;;

let deadlines t =
  match severity t, t.classified_at with
  | Minor, _ | Major, None -> None
  | Major, Some classified_at ->
    let initial =
      Time_ns.min
        (Time_ns.add classified_at (Time_ns.Span.of_hr 4.))
        (Time_ns.add t.detected_at (Time_ns.Span.of_hr 24.))
    in
    let initial_sent = Option.value (last_sent t Initial) ~default:initial in
    let intermediate = Time_ns.add initial_sent (Time_ns.Span.of_hr 72.) in
    let final = Option.map (last_sent t Intermediate) ~f:add_month in
    Some { Deadlines.initial; intermediate; final }
;;

let overdue t ~now =
  match deadlines t with
  | None -> []
  | Some d ->
    let sent kind = Option.is_some (last_sent t kind) in
    List.filter_map
      [ Report.Initial, Some d.initial
      ; Intermediate, Some d.intermediate
      ; Final, d.final
      ]
      ~f:(fun (kind, due) ->
        match due with
        | Some due when (not (sent kind)) && Time_ns.( > ) now due -> Some (kind, due)
        | _ -> None)
;;
