# AGENTS.md (types/)

The `neobank-api-types` package: request and response types of the HTTP API, with yojson
and core only. `neobank-web` depends on this package alone, so it must not pull in the server.

- A change here is a change to the web client too. Build and test `neobank-web` after editing
  a type.
- Keep the package free of effects and of anything from `lib/`.

Build: `opam exec --switch 5.2.0+ox -- dune build --root .. @neobank-api/all`.
