open! Core

module Key = struct
  type t = string * (string * string) list [@@deriving compare, hash, sexp]
end

(* Request latency buckets in seconds. *)
let buckets = [ 0.005; 0.01; 0.025; 0.05; 0.1; 0.25; 0.5; 1.; 2.5; 5.; 10. ]

type histogram =
  { counts : int array
  ; mutable sum : float
  ; mutable n : int
  }

type t =
  { counters : (Key.t, int) Hashtbl.t
  ; mutable gauges : (string * (unit -> float)) list
  ; histograms : (Key.t, histogram) Hashtbl.t
  }

let create () =
  { counters = Hashtbl.create (module Key)
  ; gauges = []
  ; histograms = Hashtbl.create (module Key)
  }
;;

let incr ?(labels = []) ?(by = 1) t name =
  Hashtbl.update t.counters (name, labels) ~f:(fun n -> Option.value n ~default:0 + by)
;;

let gauge t name f = t.gauges <- (name, f) :: t.gauges

let request t ~route ~meth ~status ~duration =
  incr
    t
    "http_requests"
    ~labels:[ "route", route; "method", meth; "status", Int.to_string status ];
  let key = "http_request_duration_seconds", [ "route", route ] in
  let h =
    Hashtbl.find_or_add t.histograms key ~default:(fun () ->
      { counts = Array.create ~len:(List.length buckets) 0; sum = 0.; n = 0 })
  in
  let s = Time_ns.Span.to_sec duration in
  List.iteri buckets ~f:(fun i b ->
    if Float.( <= ) s b then h.counts.(i) <- h.counts.(i) + 1);
  h.sum <- h.sum +. s;
  h.n <- h.n + 1
;;

let count ?(labels = []) t name =
  Option.value (Hashtbl.find t.counters (name, labels)) ~default:0
;;

let escape v =
  String.concat_map v ~f:(function
    | '"' -> "\\\""
    | '\\' -> "\\\\"
    | '\n' -> "\\n"
    | c -> String.of_char c)
;;

let labels_text = function
  | [] -> ""
  | labels ->
    "{"
    ^ String.concat
        ~sep:","
        (List.map labels ~f:(fun (k, v) -> sprintf "%s=\"%s\"" k (escape v)))
    ^ "}"
;;

let render t =
  let buf = Buffer.create 4096 in
  let line fmt =
    ksprintf
      (fun s ->
        Buffer.add_string buf s;
        Buffer.add_char buf '\n')
      fmt
  in
  Hashtbl.to_alist t.counters
  |> List.sort ~compare:(fun (a, _) (b, _) -> Key.compare a b)
  |> List.iter ~f:(fun ((name, labels), n) ->
    line "neobank_%s_total%s %d" name (labels_text labels) n);
  List.sort t.gauges ~compare:(fun (a, _) (b, _) -> String.compare a b)
  |> List.iter ~f:(fun (name, f) -> line "neobank_%s %g" name (f ()));
  Hashtbl.to_alist t.histograms
  |> List.sort ~compare:(fun (a, _) (b, _) -> Key.compare a b)
  |> List.iter ~f:(fun ((name, labels), h) ->
    List.iteri buckets ~f:(fun i b ->
      line
        "neobank_%s_bucket%s %d"
        name
        (labels_text (labels @ [ "le", Float.to_string b ]))
        h.counts.(i));
    line "neobank_%s_bucket%s %d" name (labels_text (labels @ [ "le", "+Inf" ])) h.n;
    line "neobank_%s_sum%s %g" name (labels_text labels) h.sum;
    line "neobank_%s_count%s %d" name (labels_text labels) h.n);
  Buffer.contents buf
;;
