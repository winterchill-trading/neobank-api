open! Core
open Neobank_api

let%expect_test "the default policy loads and covers every account currency" =
  let policy = Risk_policy.default in
  print_s [%sexp (Risk_rules.Policy.tiers policy : Risk_rules.Tier.t list)];
  [%expect {| (basic verified under_review) |}];
  (* Each cap lists every currency the API opens accounts in, or it would cap the missing
     ones at zero. *)
  let text = Risk_policy.default_text in
  List.iter Ctx.Config.default.currencies ~f:(fun c ->
    let code = Money.Currency.to_string c in
    let mentions =
      List.length
        (String.substr_index_all text ~may_overlap:false ~pattern:(" " ^ code ^ "\""))
    in
    printf "%s %d\n" code mentions);
  [%expect
    {|
    EUR 10
    BTC 8
    BTC@lightning 6
    ETH 6
    USDC 8
    USDT 8
    |}]
;;
