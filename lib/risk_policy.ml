open! Core

(* Amounts are "<decimal> <currency>" with every fractional digit of the currency. A
   currency missing from a list is capped at zero, so every list names all six. Above the
   first cap a payment is held for an operator; above the second it is refused. *)
let default_text =
  {|((all_tiers
  ((Blocked_countries (KP IR SY CU))
   (High_risk_countries (AF MM))
   (Velocity (max_count 10) (window 10m))
   (Structuring
    (threshold ("10000.00 EUR"))
    (round_unit ("1000.00 EUR"))
    (margin_bps 500)
    (min_count 3)
    (window 7d))
   (Mule_pass_through (window 1h) (min_incoming ("200.00 EUR")))
   (New_beneficiary_cooldown
    (cooldown 24h)
    (above ("2500.00 EUR" "0.05000000 BTC" "2500.000000 USDC" "2500.000000 USDT")))
   (Severity Block
    (Account_takeover (recent 24h) (min_signals 4) (balance_share_bps 9000)))
   (App_first_large_payment (new_within 48h))
   Vop_not_matched
   (Unverified_self_hosted_wallet
    (above ("0.01000000 BTC" "1000.000000 USDC" "1000.000000 USDT")))
   (Sca_tra (scheme Credit_transfer) (observed_fraud_rate_ppm 40))))
 (tiers
  ((basic
    ((Severity Review
      (Per_transfer_cap
       ("500.00 EUR" "0.00500000 BTC" "0.200000000000000000 ETH"
        "500.000000 USDC" "500.000000 USDT" "0.00500000000 BTC@lightning")))
     (Per_transfer_cap
      ("1000.00 EUR" "0.01000000 BTC" "0.500000000000000000 ETH"
       "1000.000000 USDC" "1000.000000 USDT" "0.01000000000 BTC@lightning"))
     (Daily_cap
      ("2000.00 EUR" "0.02000000 BTC" "1.000000000000000000 ETH"
       "2000.000000 USDC" "2000.000000 USDT" "0.02000000000 BTC@lightning"))))
   (verified
    ((Severity Review
      (Per_transfer_cap
       ("5000.00 EUR" "0.10000000 BTC" "2.000000000000000000 ETH"
        "5000.000000 USDC" "5000.000000 USDT" "0.05000000000 BTC@lightning")))
     (Per_transfer_cap
      ("15000.00 EUR" "0.50000000 BTC" "5.000000000000000000 ETH"
       "15000.000000 USDC" "15000.000000 USDT" "0.10000000000 BTC@lightning"))
     (Daily_cap
      ("25000.00 EUR" "1.00000000 BTC" "10.000000000000000000 ETH"
       "25000.000000 USDC" "25000.000000 USDT" "0.20000000000 BTC@lightning"))))
   (under_review ((Manual_review (reason "customer under investigation")))))))|}
;;

let default = Risk_rules.Policy.of_string default_text |> Or_error.ok_exn

let tier_of_kyc : Kyc_core.Tier.t -> Risk_rules.Tier.t = function
  | T0 | T1 -> Risk_rules.Tier.of_string "basic"
  | T2 | T3 -> Risk_rules.Tier.of_string "verified"
;;
