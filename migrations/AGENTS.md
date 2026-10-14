# AGENTS.md (migrations/)

PostgreSQL migrations, applied in file-name order. Each file is `<version>_<name>.sql`.

- Never edit a migration that has shipped; add the next version instead.
- `lib/gen/` embeds these files into the binary, so a new file needs no code change, but it
  must build and run against both the memory and the PostgreSQL store (the curl walk in CI).
