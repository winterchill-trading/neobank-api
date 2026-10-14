# API notes

What this API promises its clients, and what we learned wiring it to the provider
boundary (`banking-provider`) and its fake.

## Our API

- **Authentication.** `POST /auth/login` takes e-mail, password and an authenticator
  code and returns a bearer token. The first successful login confirms the authenticator
  enrolled at sign-up (the secret is returned once, by `/auth/signup`, with an
  `otpauth://` URI for a QR code). A session ends after 5 minutes without a request or
  12 hours in all; the answer is 401 `session_expired` (or `no_session` once pruned).
- **Fresh SCA.** Routes that reveal or change sensitive things answer 403
  `sca_required` when the last login or `POST /auth/step-up` is more than 5 minutes old.
  Payments do not use this: each has its own challenge, bound to amount and payee.
- **Two kinds of challenge.** Ours (factors `password` and `totp`), answered on
  `/transfers/:id/confirm-sca` with both; and the provider's (factor `otp`), which the
  provider may add to a payment, swap, standing order or card-number display. A
  payment's provider challenge is answered on the same transfer route with `code`
  alone; the others on `POST /sca/:challenge_id`. `202` with
  `{"error": "sca_required", "challenge": {...}}` announces one.
- **Versioning.** None yet: one version, additive changes only (new optional fields,
  new routes). Clients must ignore unknown fields; the shared types do
  (`allow_extra_fields`).
- **Idempotency.** See the README. Keys are 1 to 255 printable ASCII characters; UUIDs
  are a good choice. A retried request must send the same body: the hash is over the
  canonical JSON (keys sorted, no spaces), so spacing and key order do not matter.
- **Pagination.** Lists are complete and newest first for transfers and swaps. Fine for
  a demo customer; a real one would need cursors (`?before=<id>`).
- **Rate limits.** None besides the SCA lock (five failed answers lock for 30 minutes,
  a second lock within a day is permanent until re-enabled).
- **Webhooks in.** `POST /webhooks/provider` hands the raw bytes and headers to the
  provider adapter, which checks the signature before parsing. 200 with the number of
  new and repeated events; 401 for a bad signature; 500 when applying failed, which
  makes the provider redeliver (the inbox skips what already applied).
- **Pushes out.** `/ws?token=` sends the customer's own changes as
  `{"type", "data"}` objects; nothing is expected from the client.

## The provider boundary

- **A payment goes to the provider as drafted.** `Provider.S.initiate_transfer` takes a
  `Draft` (the fake refuses anything else with "only outgoing drafts can be
  initiated"), while `payments-core` moves a transfer to `Awaiting_sca` when a challenge
  is issued. The API keeps its SCA bookkeeping on its own copy and hands the provider
  the transfer with status `Draft` and the recorded authorization.
- **Results come as events, not answers.** The fake publishes events on its stream as
  it works (a deposit, a KYC decision, a settlement), so an API call's effects are
  visible only after those events are applied. Handlers that act on the provider wait
  for the events published so far (`Ctx.settle`): the event consumer queues every
  event on one sequencer as soon as it reads it, so waiting behind that sequencer after
  `Scheduler.yield_until_no_jobs_remain` covers everything published. Consuming with
  `Pipe.iter` (waiting for each event before reading the next) made that wait return
  early with PostgreSQL behind the store, because the database round trips are not
  Async jobs.
- **Two routes for one event.** The same event arrives on the stream and, for a real
  provider, as a webhook; both go through the inbox, so whichever comes second is a
  no-op.
- **Replay window.** The fake signs deliveries with a timestamp (`t=...,v1=...`) and
  rejects them more than 5 minutes after signing; a webhook replayed later is refused at
  the signature check, before the inbox.
- **Opening accounts is not idempotent** at the provider boundary (no request id), so
  the outbox job lists the customer's provider accounts first and opens only the
  currencies missing. Enrichment (IBAN, deposit address) may come later; the account
  event carries it.
- **Standing order runs** arrive as `Standing_order_run` with the provider's transfer
  id; the API creates the run's transfer from the order (exempt as a recurring payment,
  RTS art. 14) so the transfer's later events have a record to land on.
- **Sandbox code.** The fake answers `123456` to all its own challenges and codes.

## Ledger

The fake books into a `ledger` journal under `-state-dir`. With a frozen clock the
journal restamps entries strictly increasing, so tests move the synthetic clock a second
per request. The projection into PostgreSQL resumes from the highest projected sequence
number and skips records already there, so it can run on every relay pass.
