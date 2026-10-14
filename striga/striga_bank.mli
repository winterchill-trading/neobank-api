(** [-provider striga]: the API's {!Neobank_api.Bank.t} over striga-provider's adapter,
    with the links the adapter must remember kept in the API's own store (table
    [provider_links]) and the card-number component's browser options from the [STRIGA_*]
    settings. Credentials come only from the environment. *)

open! Core

(** The adapter's {!Striga_provider.Store.t} over the API's store. Each link is one row by
    kind ([striga.customer], [striga.transfer], ...) and key. A store failure raises: the
    adapter's interface has no error channel, and the request fails as a 500. *)
val links : Neobank_api.Any_store.t -> now:(unit -> Time_ns.t) -> Striga_provider.Store.t

val create
  :  store:Neobank_api.Any_store.t
  -> now:(unit -> Time_ns.t)
  -> unit
  -> Neobank_api.Bank.t Or_error.t
