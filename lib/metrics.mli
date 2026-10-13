(** Counters, gauges and request latency, rendered in the Prometheus text format for
    [/metrics]. Labels are fixed strings chosen by the code (route names, event kinds),
    never customer data. *)

open! Core

type t

val create : unit -> t

(** [incr t name] adds one to [neobank_<name>_total]; [labels] become Prometheus labels. *)
val incr : ?labels:(string * string) list -> ?by:int -> t -> string -> unit

(** A value read at render time. *)
val gauge : t -> string -> (unit -> float) -> unit

val request
  :  t
  -> route:string
  -> meth:string
  -> status:int
  -> duration:Time_ns.Span.t
  -> unit

(** Current value of a counter, for tests. *)
val count : ?labels:(string * string) list -> t -> string -> int

val render : t -> string
