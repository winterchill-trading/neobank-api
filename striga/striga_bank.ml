open! Core
open Async
open Neobank_api
module S = Striga_provider.Store
module Adapter = Striga_provider.Adapter

let links store ~now : S.t =
  let get kind key ~f =
    match%map Any_store.provider_link store ~kind ~key with
    | Ok None -> None
    | Ok (Some v) -> Some (f v)
    | Error e -> Error.raise e
  in
  let set kind key value =
    match%map
      Any_store.commit store [ Provider_link { kind; key; value; at = now () } ]
    with
    | Ok _ -> ()
    | Error e -> Error.raise e
  in
  let bin writer v = Bin_prot.Writer.to_string writer v in
  let unbin reader s = Bin_prot.Reader.of_string reader s in
  let transfer_of = unbin S.Transfer_record.bin_reader_t in
  let set_transfer (r : S.Transfer_record.t) =
    let%bind () =
      set
        "striga.transfer"
        (Payments_core.Transfer.Id.to_string r.id)
        (bin S.Transfer_record.bin_writer_t r)
    in
    match r.provider_ref with
    | None -> return ()
    | Some ref_ ->
      set "striga.transfer_ref" ref_ (Payments_core.Transfer.Id.to_string r.id)
  in
  { find_customer =
      (fun id ->
        get
          "striga.customer"
          (Kyc_core.Customer_id.to_string id)
          ~f:Banking_provider.Customer_ref.of_string)
  ; set_customer =
      (fun id r ->
        set
          "striga.customer"
          (Kyc_core.Customer_id.to_string id)
          (Banking_provider.Customer_ref.to_string r))
  ; find_transfer =
      (fun id ->
        get "striga.transfer" (Payments_core.Transfer.Id.to_string id) ~f:transfer_of)
  ; find_transfer_by_ref =
      (fun ref_ ->
        match%bind get "striga.transfer_ref" ref_ ~f:Fn.id with
        | None -> return None
        | Some id -> get "striga.transfer" id ~f:transfer_of)
  ; set_transfer
  ; (* Challenge records hold no personal data; their sexp is their stored form. *)
    find_challenge =
      (fun id ->
        get "striga.challenge" (Payments_core.Sca.Id.to_string id) ~f:(fun s ->
          S.Challenge_record.t_of_sexp (Sexp.of_string s)))
  ; set_challenge =
      (fun r ->
        set
          "striga.challenge"
          (Payments_core.Sca.Id.to_string r.challenge.id)
          (Sexp.to_string [%sexp (r : S.Challenge_record.t)]))
  ; find_swap =
      (fun id ->
        get
          "striga.swap"
          (Payments_core.Swap.Id.to_string id)
          ~f:(unbin S.Swap_record.bin_reader_t))
  ; set_swap =
      (fun r ->
        set
          "striga.swap"
          (Payments_core.Swap.Id.to_string r.id)
          (bin S.Swap_record.bin_writer_t r))
  ; find_card_request =
      (fun key -> get "striga.card_request" key ~f:Card_core.Ids.Card_id.of_string)
  ; set_card_request =
      (fun key id -> set "striga.card_request" key (Card_core.Ids.Card_id.to_string id))
  }
;;

let card_ui adapter : Bank.Card_ui.t =
  let config = Striga_client.Transport.config (Adapter.transport adapter) in
  { display =
      (fun id ->
        let open Deferred.Or_error.Let_syntax in
        let%map info = Adapter.card adapter id in
        { Bank.Card_ui.provider = "striga"
        ; (* The component's script is loaded by the web app from the provider's
             documented location. *)
          script_url = None
        ; application_id = config.application_id
        ; ui_secret =
            Option.map config.ui_secret ~f:(fun s ->
              Banking_provider.Sensitive.of_string (Striga_client.Secret.reveal s))
        ; user_id = Some (Striga_provider.Refs.user_id info.customer)
        ; card_id = Card_core.Ids.Card_id.to_string id
        })
  ; consent_started =
      (fun id ~challenge_id -> Adapter.card_consent_started adapter id ~challenge_id)
  }
;;

let create ~store ~now () =
  let open Or_error.Let_syntax in
  let%map adapter = Adapter.of_env ~store:(links store ~now) () in
  Bank.of_provider
    (module Adapter)
    ~sandbox:(module Adapter)
    ~card_ui:(card_ui adapter)
    adapter
;;
