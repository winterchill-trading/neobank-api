# AGENTS.md (.github/)

`workflows/ci.yml` installs `libpq-dev` and PostgreSQL, pins the sibling packages, runs
`shellcheck`, the build, the tests and the curl walk on both stores, and calls the shared
`obj-magic` workflow.

- A new sibling dependency needs a line in the `pins:` block, in dependency order.
- Check a run with `gh run list --limit 1`; at most one `gh run watch <id> --exit-status`.
