open! Core
open Neobank_api

let%expect_test "masks" =
  List.iter
    [ Redact.pan "4242 4242 4242 4242"
    ; Redact.pan "12"
    ; Redact.iban "DE89 3704 0044 0532 0130 00"
    ; Redact.email "alice@example.com"
    ; Redact.email "nobody"
    ; Redact.crypto_address "bc1qar0srrr7xfkvy5l643lydnw9re59gtzzwf5mdq"
    ; Redact.secret "123456"
    ; Redact.destination "FR14 2004 1010 0505 0001 3M02 606"
    ; Redact.destination "pay@wallet.example"
    ; Redact.destination "0x52908400098527886E0F7030069857D2E4169EE7"
    ]
    ~f:print_endline;
  [%expect
    {|
    **** 4242
    ****
    DE** **** 3000
    a***@example.com
    ***
    bc1qar...5mdq
    <redacted>
    FR** **** 2606
    p***@wallet.example
    0x5290...9EE7
    |}]
;;
