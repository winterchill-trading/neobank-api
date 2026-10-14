open! Core
open Neobank_api

let at s = Time_ns.of_string_with_utc_offset s

let base : Incident.t =
  { id = "inc-1"
  ; detected_at = at "2026-10-14 02:00:00Z"
  ; classified_at = Some (at "2026-10-14 09:00:00Z")
  ; critical_service_affected = true
  ; services_affected = [ "payments" ]
  ; clients_affected = 50
  ; clients_of_service = 10_000
  ; duration = Time_ns.Span.of_hr 1.
  ; downtime = Time_ns.Span.of_min 30.
  ; member_states = [ "DE" ]
  ; data_impact = false
  ; reputational_impact = false
  ; costs_eur = Money.Amount.of_string "500.00 EUR"
  ; root_cause = "provider outage"
  ; status = Open
  ; reports = []
  }
;;

let show (t : Incident.t) =
  print_s
    [%message
      ""
        ~criteria:(Incident.criteria_met t : Incident.Criterion.t list)
        ~severity:(Incident.severity t : Incident.Severity.t)
        ~deadlines:(Incident.deadlines t : Incident.Deadlines.t option)]
;;

let%expect_test "classification" =
  show base;
  [%expect {| ((criteria ()) (severity Minor) (deadlines ())) |}];
  (* One criterion is not enough; two are. *)
  show { base with downtime = Time_ns.Span.of_hr 3. };
  [%expect {| ((criteria (Duration)) (severity Minor) (deadlines ())) |}];
  show { base with downtime = Time_ns.Span.of_hr 3.; member_states = [ "DE"; "FR" ] };
  [%expect
    {|
    ((criteria (Duration Geography)) (severity Major)
     (deadlines
      (((initial "2026-10-14 13:00:00Z") (intermediate "2026-10-17 13:00:00Z")
        (final ())))))
    |}];
  (* Data loss alone is enough, but only for a critical service. *)
  show { base with data_impact = true };
  [%expect
    {|
    ((criteria (Data_loss)) (severity Major)
     (deadlines
      (((initial "2026-10-14 13:00:00Z") (intermediate "2026-10-17 13:00:00Z")
        (final ())))))
    |}];
  show { base with data_impact = true; critical_service_affected = false };
  [%expect {| ((criteria (Data_loss)) (severity Minor) (deadlines ())) |}];
  show
    { base with
      clients_affected = 1_001
    ; costs_eur = Money.Amount.of_string "100000.01 EUR"
    };
  [%expect
    {|
    ((criteria (Clients Economic)) (severity Major)
     (deadlines
      (((initial "2026-10-14 13:00:00Z") (intermediate "2026-10-17 13:00:00Z")
        (final ())))))
    |}]
;;

let%expect_test "deadlines: 4 h after classification, capped at 24 h after detection" =
  let major = { base with data_impact = true } in
  show { major with classified_at = Some (at "2026-10-15 00:00:00Z") };
  [%expect
    {|
    ((criteria (Data_loss)) (severity Major)
     (deadlines
      (((initial "2026-10-15 02:00:00Z") (intermediate "2026-10-18 02:00:00Z")
        (final ())))))
    |}];
  let reported =
    { major with
      reports =
        [ { kind = Initial; sent_at = at "2026-10-14 10:00:00Z" }
        ; { kind = Intermediate; sent_at = at "2026-10-16 10:00:00Z" }
        ]
    }
  in
  show reported;
  [%expect
    {|
    ((criteria (Data_loss)) (severity Major)
     (deadlines
      (((initial "2026-10-14 13:00:00Z") (intermediate "2026-10-17 10:00:00Z")
        (final ("2026-11-16 10:00:00Z"))))))
    |}];
  print_s
    [%sexp
      (Incident.overdue major ~now:(at "2026-10-14 14:00:00Z")
       : (Incident.Report.kind * Time_ns.Alternate_sexp.t) list)];
  [%expect {| ((Initial "2026-10-14 13:00:00Z")) |}];
  print_s
    [%sexp
      (Incident.overdue reported ~now:(at "2026-12-01 00:00:00Z")
       : (Incident.Report.kind * Time_ns.Alternate_sexp.t) list)];
  [%expect {| ((Final "2026-11-16 10:00:00Z")) |}]
;;
