# AGENTS.md (scripts/)

- `ci-walk.sh MODE` starts `neobank-api` on the fake provider with the given store (`memory`,
  or `postgres` using `DATABASE_URL`), runs `curl-flow.sh` against it and stops it.
- `curl-flow.sh` walks a customer through sign-up, login with an authenticator code, KYC,
  accounts, a simulated deposit, a swap, a SEPA payment confirmed with SCA, a card, a standing
  order, a statement, and a payment held for an operator. It needs `curl` and `python3` only,
  with `API` and `OPS_TOKEN` set.

Both scripts must pass `shellcheck`. Use only the fake provider; never point them at a live one.
