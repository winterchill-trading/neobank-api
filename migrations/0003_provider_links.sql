-- Links a provider adapter must remember because the provider cannot hold them (our
-- customer, transfer and card request ids against the provider's own ids, challenges
-- in flight). Opaque to the API: the adapter owns the bytes.

CREATE TABLE provider_links (
  kind text NOT NULL,
  key text NOT NULL,
  value bytea NOT NULL,
  updated_at timestamptz NOT NULL,
  PRIMARY KEY (kind, key)
);
