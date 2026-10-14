# AGENTS.md (bin/)

The `neobank-api` server executable. `main.ml` parses flags, builds the provider and store,
and starts the HTTP server. It holds no business logic; put that in `../lib`.

- Flags: `-port`, `-provider` (`fake` or `striga`), `-store` (`memory` or `postgres`),
  `-database-url`, `-state-dir`, `-policy`, `-allow-origin`, `-fake-settlement`, `-fake-sca`.
- Build: `opam exec --switch 5.2.0+ox -- dune build --root .. @neobank-api/all`.
