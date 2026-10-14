open! Core
open Async
open Harness

let password = `String "correct horse battery"

let%expect_test "crypto deposits and payouts" =
  let%bind () =
    with_api (fun t ->
      let%bind secret, _eur, btc = onboard t in
      let%bind accounts = get t "/accounts" in
      let id currency =
        match accounts.json with
        | `List l ->
          List.find_map_exn l ~f:(fun a ->
            if String.equal (str a "currency") currency then Some (str a "id") else None)
        | _ -> assert false
      in
      let eth = id "ETH" in
      let lightning = id "BTC@lightning" in
      let deposit account a c =
        let%map r =
          post
            t
            "/sandbox/deposit"
            ~body:(`Assoc [ "account_id", `String account; "amount", amount a c ])
        in
        show ("deposit " ^ c) r
      in
      let%bind () = deposit btc "0.05000000" "BTC" in
      let%bind () = deposit eth "1.5" "ETH" in
      let%bind () = deposit lightning "0.001" "BTC@lightning" in
      let%bind r = get t ("/accounts/" ^ btc) in
      show "btc" r ~fields:[ "network"; "balance.available.amount" ];
      let payee rail destination =
        let%map r =
          post
            t
            "/beneficiaries"
            ~body:
              (`Assoc
                [ "name", `String "Wallet"
                ; "rail", `String rail
                ; "destination", `String destination
                ])
        in
        show ("payee " ^ rail) r ~fields:[ "destination"; "rails" ];
        str r.json "id"
      in
      let pay ?wallet_provider account payee a c =
        let%bind r =
          post
            t
            "/transfers"
            ~body:
              (`Assoc
                ([ "account_id", `String account
                 ; "beneficiary_id", `String payee
                 ; "amount", amount a c
                 ]
                 @ Option.value_map wallet_provider ~default:[] ~f:(fun w ->
                   [ "wallet_provider", `String w ])))
        in
        show ("pay " ^ c) r ~fields:[ "status"; "rail"; "error"; "message" ];
        if r.status = 201
        then (
          next_step t;
          let%bind r =
            post
              t
              ("/transfers/" ^ str r.json "id" ^ "/confirm-sca")
              ~body:(`Assoc [ "password", password; "code", `String (totp t secret) ])
          in
          let%map r =
            if String.equal (str r.json "provider_sca") "<none>"
            then return r
            else
              post
                t
                ("/transfers/" ^ str r.json "id" ^ "/confirm-sca")
                ~body:(`Assoc [ "code", `String "123456" ])
          in
          show "  confirmed" r ~fields:[ "status"; "error"; "message" ])
        else return ()
      in
      let%bind cold =
        payee "onchain:bitcoin" "bc1qar0srrr7xfkvy5l643lydnw9re59gtzzwf5mdq"
      in
      let%bind () = pay ~wallet_provider:"Example Exchange" btc cold "0.00100000" "BTC" in
      let%bind friend =
        payee "onchain:ethereum" "0x52908400098527886E0F7030069857D2E4169EE7"
      in
      let%bind () = pay eth friend "0.1" "ETH" in
      (* Self-hosted and above EUR 1000: the travel rule needs proof of ownership. *)
      let%bind () = pay eth friend "0.5" "ETH" in
      let%bind ln = payee "lightning" "alice@wallet.example" in
      let%bind () = pay lightning ln "0.0001" "BTC@lightning" in
      let%bind r = get t "/transfers" in
      (match r.json with
       | `List l ->
         List.iter l ~f:(fun x ->
           printf
             "  %s %s %s %s\n"
             (str x "direction")
             (str x "amount.amount")
             (str x "rail")
             (str x "status"))
       | _ -> ());
      return ())
  in
  [%expect
    {|
    deposit BTC: 201
    deposit ETH: 201
    deposit BTC@lightning: 201
    btc: 200 network=bitcoin balance.available.amount=0.05000000
    payee onchain:bitcoin: 201 destination=bc1qar...5mdq rails=["onchain:bitcoin"]
    pay BTC: 201 status=awaiting_sca rail=onchain:bitcoin error=<none> message=<none>
      confirmed: 200 status=settled error=<none> message=<none>
    payee onchain:ethereum: 201 destination=0x5290...9EE7 rails=["onchain:ethereum"]
    pay ETH: 201 status=awaiting_sca rail=onchain:ethereum error=<none> message=<none>
      confirmed: 200 status=settled error=<none> message=<none>
    pay ETH: 422 status=<none> rail=<none> error=wallet_ownership_required message=prove you control this wallet before sending more than EUR 1000 to it, or name the provider that hosts it
    payee lightning: 201 destination=a***@wallet.example rails=["lightning"]
    pay BTC@lightning: 201 status=awaiting_sca rail=lightning error=<none> message=<none>
      confirmed: 200 status=submitted error=<none> message=<none>
      out 0.00010000000 lightning submitted
      out 0.100000000000000000 onchain:ethereum settled
      out 0.00100000 onchain:bitcoin settled
      in 0.00100000000 lightning settled
      in 1.500000000000000000 onchain:ethereum settled
      in 0.05000000 onchain:bitcoin settled
      in 1000.00 sepa_instant settled
    |}];
  return ()
;;
