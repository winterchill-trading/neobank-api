# AGENTS.md

Guide for AI coding agents working in this repository. Read this first; the subdirectory
`AGENTS.md` files add detail for `lib/`, `bin/`, `types/`, `migrations/`, `scripts/`,
`striga/`, `test/` and `docs/`. The workspace-wide guide is `../oxcaml-claude-skills/AGENTS.md`.

## Purpose

`neobank-api` is the HTTP API of the neobank demo: sessions with strong customer
authentication, KYC, accounts and statements, transfers over SEPA, on-chain and Lightning
through risk rules and SCA, swaps, virtual cards, standing orders, provider webhooks,
websocket pushes and an operator review queue. It runs over the `Provider.S` of
`../banking-provider`, with the deterministic fake provider by default.

## Layout

| Path | Holds |
| --- | --- |
| `lib/` | The API library (`Neobank_api`): bank, auth, journeys, store, outbox, ops views, HTTP routes |
| `lib/gen/` | `gen_migrations`, a generator that embeds `migrations/*.sql` into `lib/` at build time |
| `bin/` | The `neobank-api` server binary and its flags (`-provider`, `-store`, `-state-dir`, ...) |
| `types/` | `neobank-api-types` package: request and response types, shared with `neobank-web` |
| `migrations/` | Versioned PostgreSQL migrations (`0001_init.sql` onward) |
| `striga/` | The provider adapter selected by `-provider striga` |
| `scripts/` | `ci-walk.sh` (curl walk against the binary) and `curl-flow.sh` (the flow it runs) |
| `test/` | Unit and property tests, and the HTTP walk on the fake provider |
| `docs/` | `api-notes.md` |
| `.github/` | CI workflow |

## Build, test, docs

Inside the workspace (sibling checkouts and the shared `5.2.0+ox` switch are required):

```sh
opam exec --switch 5.2.0+ox -- dune build --root .. @neobank-api/all
opam exec --switch 5.2.0+ox -- dune build --root .. @neobank-api/runtest
opam exec --switch 5.2.0+ox -- dune build --root .. @neobank-api/doc
```

Parallel work: prefix the commands with `DUNE_BUILD_DIR=_build-neobank-api`.

Standalone: most dependencies are not in the `winterchill` opam repository, so outside the
workspace pin them from GitHub in the order of `.github/workflows/ci.yml`, then
`dune build && dune test`. The curl walk needs `libpq` and a PostgreSQL server for the
`postgres` store; the `memory` store needs neither.

## Conventions

- Every module has an `.mli`; public values carry odoc comments. Code is `open! Core`, with
  labelled arguments and ppx_jane derivations.
- Storage goes through `Store.S`. Only `Store_pg` contains SQL. The ledger journal under
  `-state-dir` stays the system of record.
- Secrets come from the environment (`DATABASE_URL`, `NEOBANK_OPS_TOKEN`, provider `*_` variables). Never
  write a value into code, a test or a fixture.
- Formatting: `opam exec --switch 5.2.0+ox -- dune build --root .. @neobank-api/fmt --auto-promote`.
  Never run `dune fmt` at the workspace root.
- `scripts/*.sh` pass `shellcheck` in CI.

## Dependencies and dependents

- Depends on `banking-provider`, `risk-rules`, `pg-async`, `money`, `ledger`, `kyc-core`,
  `payments-core`, `card-core`, `fixed-decimal`, plus the provider adapter packages.
- Used by `neobank-web` through `neobank-api-types` only.
- CI pin order (`ci.yml`): `fixed-decimal`, `money`, `ledger`, `kyc-core`, `payments-core`,
  `card-core`, `risk-rules`, `banking-provider`, `striga-client`, `striga-provider`, `pg-async`.
  A pin is added wherever a new sibling dependency appears.

## CI

`ci.yml` installs `libpq-dev` and PostgreSQL, runs the `ox-setup` action with the pins above,
then `shellcheck`, `dune build`, `dune test`, and the curl walk twice: on the memory store and
on PostgreSQL. The shared `obj-magic` workflow runs last. Check a run with
`gh run list --limit 1`, then at most one `gh run watch <id> --exit-status`.

## Pitfalls

- `banking-provider`'s `initiate_transfer` takes a `Draft`; `payments-core` moves a transfer to
  `Awaiting_sca` when SCA is required. Keep the SCA bookkeeping on this side and send the
  transfer as `Draft` with the authorization recorded.
- On the fake provider, KYC is decided only after both hosted steps were started. A hosted
  `kyc/start` keeps the step pending until the provider reports.
- The card PAN token endpoint answers 202 with a `Sca_required` body, not an error.
- SEPA payouts to the account's own provider IBAN are refused by the sandbox; intraledger
  between two wallets of one user is a ruleset violation. Expect both in walk output.
- Outbox retry delay once overflowed; the quickcheck property in `test/` covers it.

## Where to read more

- `README.md`: endpoints, payment flow, exactly-once effects, security, storage, tests.
- `docs/api-notes.md`, and `../specs/repos/neobank-api.md`.
- `../oxcaml-claude-skills/skills/banking-standards/` and `async-http-websocket/`.
- `../oxcaml-claude-skills/GOTCHAS.md`, section "Banking libraries" and "Fiat rails".

## Never do

- Install or upgrade opam packages that change the compiler, `core`, `base`, `async` or
  `ppx_jane` on the shared switch. Run `opam install --switch 5.2.0+ox --dry-run <pkg>` first
  and stop if the plan touches any of them.
- Publish, tag or release. Never call a live provider from a test or script.
- Commit `.env` files (they are gitignored) or any credential; read values from the environment.
- Print secrets, or poll CI in a loop.
