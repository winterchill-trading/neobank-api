# AGENTS.md (lib/)

The `Neobank_api` library. Modules group into: store (`Store`, `Store_memory`, `Store_pg`,
`Any_store`), the bank over `Provider.S` (`Bank`, `Ctx`, `Model`), sessions (`Auth`,
`Password`, `Totp`), journeys (`Onboarding`, `Accounts`, `Payments`, `Swaps`, `Cards`,
`Standing_orders`), provider events (`Events`, `Relay`, `Ledger_projection`), ops and
observability (`Views`, `Redact`, `Incident`, `Metrics`, `Hub`) and the HTTP layer (`Server`).

- `Store_pg` is the only module with SQL. Adding a column means a new migration in
  `../migrations`, plus the memory store and the transcript test.
- Every outgoing transfer goes through `Risk_policy` before it reaches the provider.
- Outbox retries are bounded; keep the backoff arithmetic overflow-safe.
- `gen/` is a build-time tool; it reads `../migrations` and does not change at runtime.

Build: `opam exec --switch 5.2.0+ox -- dune build --root .. @neobank-api/all`.
