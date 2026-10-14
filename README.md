# neobank-api

The HTTP API of the neobank demo: sign-up and sessions with strong customer
authentication, KYC, EUR and crypto accounts, beneficiaries, SEPA, on-chain and
Lightning payments through risk rules and SCA, swaps, virtual cards, standing orders,
statements, provider webhooks, websocket pushes and an operator review queue.

It runs over any implementation of `banking-provider`'s `Provider.S`: the deterministic
fake (offline, no credentials, used by the tests, CI and the web app's mock mode) or
Striga's sandbox through `striga-provider`. The domain rules come from the bank-side
libraries: `kyc-core` (the KYC case and what a customer may see), `payments-core`
(transfer state machine, SCA challenges, IBANs, SEPA details), `card-core`, `risk-rules`
(every outgoing transfer is evaluated against a policy) and `ledger` (the fake bank's
books). Application state lives in PostgreSQL through `pg-async`, behind a `Store.S` of
our own.

Two opam packages:

- `neobank-api` (library `Neobank_api`, binary `neobank-api`).
- `neobank-api-types` (library `Neobank_api_types`): the request and response bodies,
  depending only on `core` and `yojson`, so the Bonsai app `neobank-web` can share them.

**Demo only.** Sandbox credentials at most, never real money. The fake provider accepts
the code `123456` for every one of its own challenges.

## Quick start

Offline, everything in memory:

```sh
export NEOBANK_OPS_TOKEN=$(openssl rand -hex 16)   # enables /ops/*
dune exec neobank-api -- -port 8080 -store memory -state-dir /tmp/neobank-state
API=http://localhost:8080 OPS_TOKEN=$NEOBANK_OPS_TOKEN scripts/curl-flow.sh
```

`scripts/curl-flow.sh` (curl and python3 only) walks one customer through the demo:
sign-up, login with password and authenticator code, KYC (codes, hosted steps, a
sandbox approval), accounts with IBAN and deposit addresses, a simulated SEPA deposit,
a EUR to BTC swap, a SEPA payment confirmed with SCA, a card (freeze, limits, number
display, a purchase), a monthly standing order, a statement as JSON and CSV, and a
payment held for an operator, approved and then confirmed. CI runs it against the
memory store and against PostgreSQL (`scripts/ci-walk.sh`).

With the local PostgreSQL from infra's compose stack (database `neobank`, role
`neobank`, development password in `infra/docs/local.md`):

```sh
(cd ../infra && PG_PORT=5433 make up-db)
export DATABASE_URL='postgres://neobank:<password>@127.0.0.1:5433/neobank?sslmode=disable'
dune exec neobank-api -- -port 8080 -state-dir ./state
```

Pending migrations (`migrations/*.sql`, embedded in the binary) are applied at startup
under pg-async's advisory lock.

## Settings

| Flag or variable | Default | Meaning |
|---|---|---|
| `-port` | 8080 | listen port; the websocket is `/ws` on the same port |
| `-provider` | `fake` | `fake` or `striga` |
| `-store` | `postgres` when `DATABASE_URL` is set, else `memory` | where application state lives |
| `-database-url`, `DATABASE_URL`, `PG*` | | PostgreSQL target; TLS is required unless the URL says `sslmode=disable` |
| `-state-dir` | `./state` | the fake bank's ledger journal (`state/journal`) |
| `-policy` | built in (`lib/risk_policy.ml`) | a `risk-rules` policy file |
| `-allow-origin` | `http://localhost:8000` | CORS origins, repeatable, `*` for any |
| `-fake-settlement` | `immediate` | fake: `immediate`, `manual` or `delayed:<seconds>` |
| `-fake-sca` | `true` | fake: ask for its own code on payments, swaps, cards |
| `-password-iterations` | 600000 | PBKDF2 rounds for new passwords (CI lowers it) |
| `NEOBANK_OPS_TOKEN` | unset | bearer token for `/ops/*`; unset hides those routes |
| `STRIGA_*` | | read by `striga-provider` with `-provider striga` |

Logs go to stderr, one line per request (method, route, status, milliseconds; never
bodies).

## Endpoints

The full table with bodies is the comment at the top of `lib/server.mli`; bodies are
`types/api_types.mli`. In short:

| Area | Routes |
|---|---|
| Health | `GET /healthz`, `GET /metrics` (Prometheus) |
| Sessions | `POST /auth/signup`, `/auth/login`, `/auth/step-up`, `/auth/logout`, `GET /me` |
| KYC | `GET /kyc`, `POST /kyc/start` (codes, or the provider SDK token), `POST /kyc/complete` |
| Accounts | `GET /accounts`, `/accounts/:id`, `/accounts/:id/statement?from=&until=&format=csv` |
| Payments | `GET/POST /beneficiaries`, `POST /beneficiaries/:id/trust`, `POST /transfers/quote`, `POST /transfers`, `GET /transfers[/:id]`, `POST /transfers/:id/confirm-sca`, `/sca`, `/cancel` |
| Swaps | `POST /swaps/quote`, `POST /swaps/:id/execute`, `GET /swaps[/:id]` |
| Cards | `GET/POST /cards`, `POST /cards/:id/freeze`, `/unfreeze`, `/limits`, `/pan-token`, `/consent`, `GET /cards/:id/display` |
| Standing orders | `GET/POST /standing-orders`, `DELETE /standing-orders/:id` |
| Provider codes | `POST /sca/:id` (the provider's own challenge for a swap, order or card number) |
| Pushes | `GET /ws?token=...` (`{"type": "transfer" | "account" | "kyc" | "card" | "swap" | "standing_order", "data": ...}`) |
| Webhooks | `POST /webhooks/provider` |
| Operators | `GET /ops/review`, `POST /ops/review/:id`, `GET /ops/outbox`, `GET /ops/audit`, `GET /ops/ledger`, `GET /ops/ledger/postings?account=` |
| Sandbox | `POST /sandbox/kyc-decision`, `/sandbox/deposit`, `/sandbox/card-auth`, `/sandbox/transfer-outcome` (only when the provider has a sandbox) |

Conventions: JSON bodies, amounts as `{"amount": "12.34", "currency": "EUR"}` with the
decimal as a string, ISO 8601 UTC times, errors as `{"error": code, "message": text}`.
Every `POST` except `/auth/*` and `/webhooks/*` needs an `Idempotency-Key` header.

## How a payment goes

1. `POST /transfers` builds a `payments-core` draft (SEPA pacs.008 details with our id as
   end-to-end id, a travel-rule record on-chain), checks the balance, and evaluates the
   risk policy over a context of the customer's history, balance and session signals.
   On-chain payments of more than EUR 1000 to a self-hosted wallet are refused until the
   customer proves control of it (travel rule); naming the provider that hosts the
   wallet (`wallet_provider`) records it as hosted.
   On-chain payments of more than EUR 1000 to a self-hosted wallet are refused until the
   customer proves control of it (travel rule); naming the provider that hosts the
   wallet (`wallet_provider`) records it as hosted.
   The context and the decision are stored with the transfer, so the decision can be
   replayed.
2. `Block`: refused (422 `payment_refused`, with risk-rules' customer message), stored
   as `Cancelled`. `Review`: held (202, `under_review`, message "under review"; the
   customer never sees reasons) and queued for `/ops/review`, where two different
   operators must approve the release (four eyes) and one may reject it. Otherwise an SCA challenge
   bound to amount, currency and payee (password and authenticator code; the RTS
   article is recorded instead when risk-rules grants an exemption).
3. `POST /transfers/:id/confirm-sca` checks both factors; a wrong answer never says
   which one, five in a row lock the customer. Verification writes the transfer and an
   outbox message in one transaction.
4. The relay sends the transfer to the provider (the transfer id is the provider's
   idempotency key). If the provider asks for its own code, the same route answers it.
5. Completion, rejection, returns and confirmations arrive as provider events and move
   the transfer through `payments-core`'s state machine; reports that were overtaken are
   ignored.

## Exactly-once effects

- **Idempotency keys** (reliability note): scope `(customer, method, route, key)`,
  SHA-256 of the canonical JSON body, stored response replayed for 24 hours, 422
  `idempotency_key_reused` for another body, 409 `idempotency_key_in_use` while the
  first request runs. Malformed requests (400) and authentication failures are not
  stored. A card's provider request id is derived from the key.
- **Outbox**: provider calls (create customer, open accounts, initiate transfer, execute
  swap, create standing order) are rows written with the state change; the relay polls
  every second, backs off from 1 s to 5 minutes, and parks a message as `Needs_review`
  after 24 hours (`GET /ops/outbox`).
- **Inbox**: every provider event's effects commit together with a row keyed by the
  provider's event id; a redelivery (or the same event by webhook and by the event
  stream) is a no-op, the same id with other content is an error. Kept 30 days.
- **Audit**: who, on whose behalf, what, when, where and the outcome, append-only and
  hash-chained (`GET /ops/audit` checks the chain).
- **Ledger**: the fake bank books every movement into a `ledger` journal under
  `-state-dir`, which stays the system of record for the books; each relay pass projects
  new journal records into `ledger_records` and `ledger_postings` for queries
  (`GET /ops/ledger` shows the trial balance, zero per currency).

## Security

- Passwords: PBKDF2-HMAC-SHA256, 600 000 rounds, 16-byte salt, in a thread. Argon2 would
  be the first choice but nothing on the OxCaml switch provides it, and cryptokit 1.16
  has no KDF; the stored form names the scheme so a later change can rehash at login.
  Checked against the RFC 7914 vectors.
- Authenticator codes: RFC 6238 (checked against its vectors), one step of drift either
  side, each step accepted once per customer.
- Sessions: 32 random bytes, only the SHA-256 is stored; 5 minutes idle, 12 hours in
  all. Revealing a card number, changing limits, swaps, standing orders and trusting a
  payee need a login or step-up within 5 minutes; payments have their own challenge.
- What leaves the API: card numbers masked to the last four, IBANs to country and last
  four in operator views and audit records (`Redact`), no risk reasons or KYC alerts
  for customers (kyc-core's customer view only), no secrets in logs or records (password
  hashes and TOTP secrets print as `<redacted>`). The card number itself is only ever
  shown through the provider's hosted component, with the token from `/pan-token`.
- Webhooks: verified by the provider adapter on the raw bytes, in constant time, before
  any parsing (the fake's scheme rejects deliveries older than 5 minutes).
- `Incident` classifies ICT incidents under DORA (major when a critical service is hit
  and data was lost or two other criteria are met) and computes the 4 h / 72 h / 1 month
  report deadlines.

## Storage

`Store.S` (`lib/store_intf.ml`) is the API's state in domain types (`lib/model.mli`):
customers, sessions, KYC events, accounts, beneficiaries, transfers, swaps, cards,
standing orders, reviews, outbox, inbox, idempotency keys, audit chain and the ledger
projection. All writes go through `commit`, which applies a batch atomically. Two
implementations: `Store_memory` and `Store_pg`, the only module with SQL (plus
`migrations/`). Each row keeps the record's Bin_prot bytes in `body` next to the columns
queries use. `test/store_suite.ml` prints one transcript of store behaviour;
`test/test_store.ml` runs it on both stores with identical expected output, and
`test/test_flow.ml` runs the whole HTTP walk on both.

The API owns its database (`neobank`); nothing joins across services. Reads never assume
they see another handle's writes.

Recovery targets for the demo: RPO 0 for acknowledged state (PostgreSQL commits before
the response; the journal fsyncs each append), RTO 15 minutes: restore the database and
the journal, start the API (migrations, then the relay resends anything not marked
sent), reconcile balances with the provider.

## Tests

`dune test` needs PostgreSQL's server binaries (`initdb`, `pg_ctl`) for the throwaway
clusters (`brew install postgresql@18`, or `apt-get install postgresql`).

| What | Verified on |
|---|---|
| PBKDF2, TOTP, base32 | RFC 7914, RFC 6238 and RFC 4648 vectors |
| Store behaviour | one transcript, memory and PostgreSQL |
| The HTTP walk (all routes above, idempotency, wrong factors, review, refusal, stale SCA, webhook redelivery and forgery, standing order run, ledger balance) | fake provider, memory and PostgreSQL |
| Crypto: BTC and ETH on-chain with travel-rule records, Lightning, the EUR 1000 self-hosted wallet rule | fake provider |
| Sign-up validation, login failures, the SCA lock, authenticator code replay | fake provider |
| Outbox backoff and the give-up to operator review | fake provider |
| Crypto: BTC and ETH on-chain with travel-rule records, Lightning, the EUR 1000 self-hosted wallet rule | fake provider |
| Sign-up validation, login failures, the SCA lock, authenticator code replay | fake provider |
| Outbox backoff and the give-up to operator review | fake provider |
| Request hash, backoff, masking | Quickcheck properties |
| DORA classification and deadlines | expect tests |
| The curl walk | CI, against the binary, memory and PostgreSQL |

Nothing is verified live: the Striga path runs only through `striga-provider`'s own
tests (see Known gaps); `-provider striga` was checked to start and to refuse cleanly
without `STRIGA_*`.

## Known gaps

- `-provider striga` is written and compiles (branch `striga-provider`: the adapter
  over striga-provider with its links in `provider_links` and the card component's
  options from `STRIGA_*`), but is not on `main`: the pushed striga-provider still
  includes `Provider.Sandbox`, which banking-provider renamed to `Sandbox.S`. Merge the
  branch once striga-provider follows.
- KYC screening and risk rating are not run by the API: the demo KYC policy trusts the
  provider's decision for them (`Ctx.Config.demo_kyc_policy`).
- Personal data in KYC events is stored as Bin_prot without per-customer encryption.
- Per-record locks are per process: run one API instance (several instances would need
  optimistic versions on the records).
- Provider challenges answered through `/sca/:id` are remembered in memory; after a
  restart the customer starts the operation again.
- No rate limiting beyond the SCA lock; no device binding (risk-rules' `new_device` is
  always false).
- Card authorizations and clearings are counted, not stored per card.
- With the fake provider, the bank's own customers live in memory: restarting the API
  over a kept database gives customers whose provider records are gone. Start with a
  fresh database and state directory, or use `-store memory`.

## Why separate

The API is the only place that knows HTTP, sessions, the database and which provider
runs; the libraries below it stay pure and reusable, and `neobank-web` depends only on
`neobank-api-types`.
