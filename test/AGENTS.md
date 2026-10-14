# AGENTS.md (test/)

Expect tests, Quickcheck properties and the HTTP walk on the fake provider.

- PBKDF2, TOTP and base32 are checked against RFC vectors.
- Store behaviour is one transcript run against both the memory and the PostgreSQL store.
- Property tests cover the request hash, outbox backoff and masking of card numbers and IBANs.
- Promote expect output with `dune promote <file>` for the one file that changed.
- Nothing here calls a live provider.

Run: `opam exec --switch 5.2.0+ox -- dune build --root .. @neobank-api/runtest`.
