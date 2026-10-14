# AGENTS.md (striga/)

The provider adapter behind `-provider striga`: it implements `Provider.S` on top of the
provider client packages named in the root `AGENTS.md` CI pins.

- Credentials come from `STRIGA_*` environment variables only.
- The adapter must not decide business outcomes; the bank does. Map provider codes to the
  bank's refusals and keep the mapping in one place.
