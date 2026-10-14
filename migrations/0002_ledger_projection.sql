-- The ledger journal (a Bin_prot file under -state-dir) is the system of record for
-- the books; these tables are its projection for queries, rebuilt from the journal
-- by sequence number and never written otherwise.

CREATE TABLE ledger_records (
  seq bigint PRIMARY KEY,
  recorded_at timestamptz NOT NULL,
  body bytea NOT NULL
);

CREATE TABLE ledger_postings (
  seq bigint NOT NULL REFERENCES ledger_records (seq),
  line integer NOT NULL,
  entry_key text NOT NULL,
  account_id text NOT NULL,
  side text NOT NULL CHECK (side IN ('debit', 'credit')),
  currency text NOT NULL,
  -- signed minor units: debits positive, credits negative
  amount_minor numeric NOT NULL,
  code text NOT NULL,
  reference text NOT NULL,
  recorded_at timestamptz NOT NULL,
  PRIMARY KEY (seq, line)
);
CREATE INDEX ledger_postings_account ON ledger_postings (account_id, seq, line);
