(lang dune 3.17)

(name neobank-api)

(generate_opam_files true)

(license MIT)

(maintainers "winterchill-trading")

(authors "winterchill-trading")

(source
 (github winterchill-trading/neobank-api))

(package
 (name neobank-api-types)
 (synopsis "Request and response types shared by the neobank API and web app")
 (depends
  (ocaml
   (>= 5.2))
  dune
  core
  ppx_jane
  ppx_yojson_conv
  yojson))

(package
 (name neobank-api)
 (synopsis
  "HTTP API and webhooks for the neobank demo over a banking provider, with PostgreSQL state")
 (depends
  (ocaml
   (>= 5.2))
  dune
  core
  core_unix
  async
  async_kernel
  async_websocket
  cohttp
  cohttp-async
  http
  cryptokit
  uri
  yojson
  ppx_jane
  ppx_yojson_conv
  zarith
  money
  ledger
  kyc-core
  payments-core
  card-core
  risk-rules
  banking-provider
  striga-client
  striga-provider
  pg-async
  (neobank-api-types
   (= :version))))
