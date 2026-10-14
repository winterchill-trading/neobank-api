#!/usr/bin/env bash
# Starts neobank-api on the fake provider with the given store (memory or postgres,
# the latter from DATABASE_URL), runs scripts/curl-flow.sh against it and stops it.
#
#   scripts/ci-walk.sh memory
set -euo pipefail

STORE=${1:-memory}
PORT=${PORT:-8080}
STATE=$(mktemp -d)
LOG=${LOG:-$STATE/api.log}
export NEOBANK_OPS_TOKEN=${NEOBANK_OPS_TOKEN:-ci-ops-token}

opam exec -- dune build ./bin/main.exe
# Fewer PBKDF2 rounds keep sign-up and login fast in CI; production uses the default.
"$(dirname "$0")/../_build/default/bin/main.exe" -port "$PORT" -store "$STORE" \
  -state-dir "$STATE/state" -password-iterations 1000 >"$LOG" 2>&1 &
API_PID=$!
trap 'kill $API_PID 2>/dev/null || true' EXIT

for _ in $(seq 1 30); do
  if curl -sf "http://localhost:$PORT/healthz" >/dev/null; then break; fi
  sleep 1
done
curl -sf "http://localhost:$PORT/healthz"
echo

if ! API="http://localhost:$PORT" OPS_TOKEN="$NEOBANK_OPS_TOKEN" "$(dirname "$0")/curl-flow.sh"; then
  echo "--- api log" >&2
  tail -50 "$LOG" >&2
  exit 1
fi
