-- neobank-api application state. Each table keeps the record's Bin_prot bytes in
-- `body` (lib/model.mli) next to the columns that queries filter and sort on. The
-- ledger journal stays outside the database; nothing here is the system of record
-- for balances.

CREATE TABLE customers (
  id text PRIMARY KEY,
  email text NOT NULL UNIQUE,
  provider_ref text UNIQUE,
  created_at timestamptz NOT NULL,
  body bytea NOT NULL
);

CREATE TABLE sessions (
  token_hash text PRIMARY KEY,
  customer_id text NOT NULL,
  last_activity timestamptz NOT NULL,
  body bytea NOT NULL
);
CREATE INDEX sessions_last_activity ON sessions (last_activity);

CREATE TABLE kyc_events (
  seq bigint GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
  customer_id text NOT NULL,
  event_id text NOT NULL,
  at timestamptz NOT NULL,
  body bytea NOT NULL,
  UNIQUE (customer_id, event_id)
);

CREATE TABLE accounts (
  id text PRIMARY KEY,
  customer_id text NOT NULL,
  currency text NOT NULL,
  updated_at timestamptz NOT NULL,
  body bytea NOT NULL
);
CREATE INDEX accounts_customer ON accounts (customer_id);

CREATE TABLE beneficiaries (
  id text PRIMARY KEY,
  seq bigint GENERATED ALWAYS AS IDENTITY,
  customer_id text NOT NULL,
  body bytea NOT NULL
);
CREATE INDEX beneficiaries_customer ON beneficiaries (customer_id, seq);

CREATE TABLE transfers (
  id text PRIMARY KEY,
  seq bigint GENERATED ALWAYS AS IDENTITY,
  customer_id text NOT NULL,
  account_id text NOT NULL,
  status text NOT NULL,
  under_review boolean NOT NULL,
  created_at timestamptz NOT NULL,
  updated_at timestamptz NOT NULL,
  body bytea NOT NULL
);
CREATE INDEX transfers_customer ON transfers (customer_id, seq);

CREATE TABLE swaps (
  id text PRIMARY KEY,
  seq bigint GENERATED ALWAYS AS IDENTITY,
  customer_id text NOT NULL,
  status text NOT NULL,
  created_at timestamptz NOT NULL,
  body bytea NOT NULL
);
CREATE INDEX swaps_customer ON swaps (customer_id, seq);

CREATE TABLE cards (
  id text PRIMARY KEY,
  seq bigint GENERATED ALWAYS AS IDENTITY,
  customer_id text NOT NULL,
  request_id text NOT NULL UNIQUE,
  status text NOT NULL,
  body bytea NOT NULL
);
CREATE INDEX cards_customer ON cards (customer_id, seq);

CREATE TABLE standing_orders (
  id text PRIMARY KEY,
  seq bigint GENERATED ALWAYS AS IDENTITY,
  customer_id text NOT NULL,
  provider_ref text,
  status text NOT NULL,
  body bytea NOT NULL
);
CREATE INDEX standing_orders_customer ON standing_orders (customer_id, seq);
CREATE INDEX standing_orders_provider_ref ON standing_orders (provider_ref);

CREATE TABLE reviews (
  id text PRIMARY KEY,
  seq bigint GENERATED ALWAYS AS IDENTITY,
  reference text NOT NULL,
  customer_id text NOT NULL,
  pending boolean NOT NULL,
  submitted_at timestamptz NOT NULL,
  body bytea NOT NULL
);
CREATE INDEX reviews_pending ON reviews (pending, seq);

-- Transactional outbox: written in the same transaction as the state change it
-- announces; the relay sends each message at least once (reliability note).
CREATE TABLE outbox (
  id text PRIMARY KEY,
  seq bigint GENERATED ALWAYS AS IDENTITY,
  aggregate_id text NOT NULL,
  kind text NOT NULL,
  attempts integer NOT NULL,
  next_attempt_at timestamptz NOT NULL,
  created_at timestamptz NOT NULL,
  sent_at timestamptz,
  state text NOT NULL,
  last_error text,
  payload bytea NOT NULL
);
CREATE INDEX outbox_due ON outbox (state, next_attempt_at, seq);

-- Inbox: one row per provider event applied, written with its effects.
CREATE TABLE inbox (
  provider_event_id text PRIMARY KEY,
  received_at timestamptz NOT NULL,
  payload_hash text NOT NULL,
  applied boolean NOT NULL DEFAULT true
);
CREATE INDEX inbox_received_at ON inbox (received_at);

-- API idempotency keys, scoped to (customer, method, route, key); kept 24 hours.
CREATE TABLE idempotency (
  scope text PRIMARY KEY,
  customer_id text NOT NULL,
  method text NOT NULL,
  route text NOT NULL,
  key text NOT NULL,
  request_hash text NOT NULL,
  state text NOT NULL CHECK (state IN ('in_progress', 'completed')),
  status integer,
  body text,
  created_at timestamptz NOT NULL
);
CREATE INDEX idempotency_created_at ON idempotency (created_at);

-- Append-only audit trail, hash-chained (hash = sha256(prev_hash || record bytes)).
CREATE TABLE audit (
  seq bigint PRIMARY KEY,
  at timestamptz NOT NULL,
  actor text NOT NULL,
  on_behalf_of text,
  action text NOT NULL,
  object_id text NOT NULL,
  source text NOT NULL,
  outcome text NOT NULL,
  prev_hash text NOT NULL,
  hash text NOT NULL,
  body bytea NOT NULL
);
