#!/usr/bin/env bash
# Walks a customer through the demo with curl against a running neobank-api:
# sign-up, login (password + authenticator), KYC, accounts, a simulated deposit, a swap,
# a SEPA payment confirmed with SCA, a card, a standing order, a statement, and a
# payment held for an operator. Needs only curl and python3 (standard library).
#
#   API=http://localhost:8080 OPS_TOKEN=... scripts/curl-flow.sh
#
# Against -provider fake the provider's own codes are the sandbox code 123456.
set -euo pipefail

API=${API:-http://localhost:8080}
OPS_TOKEN=${OPS_TOKEN:-}
PROVIDER_CODE=${PROVIDER_CODE:-123456}
EMAIL=${EMAIL:-demo-$(date +%s)-$RANDOM@example.com}
PASSWORD=${PASSWORD:-correct horse battery staple}
PYTHON=${PYTHON:-python3}

say() { printf '\n== %s\n' "$*" >&2; }

# json <expression>: prints the Python expression over d, the JSON read from stdin.
json() { "$PYTHON" -I -c "import json,sys; d=json.load(sys.stdin); print($1)"; }

# totp <base32 secret> <step offset>: the RFC 6238 code for the current 30 s step plus
# the offset. The API accepts one step either side and each step once, so successive
# confirmations use offsets -1, 0 and 1 instead of waiting for the clock.
totp() {
  "$PYTHON" -I -c '
import base64, hashlib, hmac, struct, sys, time
secret = sys.argv[1]
key = base64.b32decode(secret + "=" * (-len(secret) % 8))
counter = int(time.time()) // 30 + int(sys.argv[2])
mac = hmac.new(key, struct.pack(">Q", counter), hashlib.sha1).digest()
o = mac[19] & 15
print("%06d" % ((struct.unpack(">I", mac[o:o + 4])[0] & 0x7fffffff) % 1000000))
' "$1" "$2"
}

key() { "$PYTHON" -I -c 'import uuid; print(uuid.uuid4())'; }

# call METHOD PATH [BODY]: prints the response body; fails on HTTP errors.
call() {
  local method=$1 path=$2 body=${3:-}
  local args=(-sS -X "$method" "$API$path" -H 'content-type: application/json' -w '\n%{http_code}')
  [ -n "${TOKEN:-}" ] && args+=(-H "authorization: Bearer $TOKEN")
  [ "$method" = POST ] && args+=(-H "idempotency-key: $(key)")
  [ -n "$body" ] && args+=(-d "$body")
  local out code
  out=$(curl "${args[@]}")
  code=${out##*$'\n'}
  out=${out%$'\n'*}
  if [ "$code" -ge 400 ]; then
    echo "$method $path -> $code $out" >&2
    return 1
  fi
  echo "$out"
}

ops() {
  local method=$1 path=$2 body=${3:-}
  local args=(-sSf -X "$method" "$API$path" -H 'content-type: application/json'
    -H "authorization: Bearer $OPS_TOKEN")
  [ "$method" = POST ] && args+=(-H "idempotency-key: $(key)")
  [ -n "$body" ] && args+=(-d "$body")
  curl "${args[@]}"
}

# poll PATH EXPRESSION: until the expression over d prints True, for up to 20 s.
poll() {
  local path=$1 expr=$2
  for _ in $(seq 1 40); do
    if [ "$(call GET "$path" | json "$expr")" = True ]; then return 0; fi
    sleep 0.5
  done
  echo "timed out waiting for $path $expr" >&2
  return 1
}

say "sign up $EMAIL"
SIGNUP=$(call POST /auth/signup "{\"email\":\"$EMAIL\",\"password\":\"$PASSWORD\",
  \"given_name\":\"Demo\",\"family_name\":\"Customer\",\"date_of_birth\":\"1990-01-01\",
  \"nationality\":\"DE\",\"mobile\":\"+4930000000\",
  \"address\":{\"line1\":\"1 Test Street\",\"city\":\"Berlin\",\"postal_code\":\"10115\",\"country\":\"DE\"}}")
SECRET=$(echo "$SIGNUP" | json "d['totp_secret']")

say "log in with password and authenticator code"
TOKEN=$(call POST /auth/login "{\"email\":\"$EMAIL\",\"password\":\"$PASSWORD\",\"totp\":\"$(totp "$SECRET" -1)\"}" \
  | json "d['token']")

say "KYC: e-mail and mobile codes, then the hosted identity steps"
call POST /kyc/start '{"step":"email"}' >/dev/null || { sleep 1; call POST /kyc/start '{"step":"email"}' >/dev/null; }
call POST /kyc/complete "{\"step\":\"email\",\"code\":\"$PROVIDER_CODE\"}" >/dev/null
call POST /kyc/start '{"step":"mobile"}' >/dev/null
call POST /kyc/complete "{\"step\":\"mobile\",\"code\":\"$PROVIDER_CODE\"}" >/dev/null
call POST /kyc/start '{"step":"identity_document"}' | json "d['kind']"
call POST /kyc/start '{"step":"liveness"}' >/dev/null
call POST /sandbox/kyc-decision '{"decision":"approve"}' | json "d['status']"

say "accounts"
poll /accounts "any(a.get('iban') for a in d)"
ACCOUNTS=$(call GET /accounts)
EUR=$(echo "$ACCOUNTS" | json "[a['id'] for a in d if a['currency']=='EUR'][0]")
BTC=$(echo "$ACCOUNTS" | json "[a['id'] for a in d if a['currency']=='BTC'][0]")
echo "$ACCOUNTS" | json "json.dumps(d)" | head -c 600; echo

say "simulated SEPA deposit of 1000.00 EUR"
call POST /sandbox/deposit "{\"account_id\":\"$EUR\",\"amount\":{\"amount\":\"1000.00\",\"currency\":\"EUR\"}}" >/dev/null
poll "/accounts/$EUR" "d['balance']['available']['amount'] == '1000.00'"

say "swap 100.00 EUR to BTC"
SWAP=$(call POST /swaps/quote "{\"from_account_id\":\"$EUR\",\"to_account_id\":\"$BTC\",
  \"sell\":{\"amount\":\"100.00\",\"currency\":\"EUR\"}}" | json "d['id']")
STATUS=$(call POST "/swaps/$SWAP/execute" | json "d['status']")
if [ "$STATUS" = awaiting_sca ]; then
  CHALLENGE=$(call GET "/swaps/$SWAP" | json "d['provider_sca']['challenge_id']")
  call POST "/sca/$CHALLENGE" "{\"code\":\"$PROVIDER_CODE\"}" >/dev/null
fi
poll "/swaps/$SWAP" "d['status'] == 'executed'"

say "SEPA payment of 30.00 EUR, confirmed with password and authenticator code"
PAYEE=$(call POST /beneficiaries '{"name":"Payee","rail":"sepa","destination":"FR1420041010050500013M02606"}' \
  | json "d['id']")
CODE=$(totp "$SECRET" 0)
TRANSFER=$(call POST /transfers "{\"account_id\":\"$EUR\",\"beneficiary_id\":\"$PAYEE\",
  \"amount\":{\"amount\":\"30.00\",\"currency\":\"EUR\"},\"reference\":\"Invoice 42\"}" | json "d['id']")
STATE=$(call POST "/transfers/$TRANSFER/confirm-sca" "{\"password\":\"$PASSWORD\",\"code\":\"$CODE\"}")
if [ "$(echo "$STATE" | json "d.get('provider_sca') is not None")" = True ]; then
  call POST "/transfers/$TRANSFER/confirm-sca" "{\"code\":\"$PROVIDER_CODE\"}" >/dev/null
fi
poll "/transfers/$TRANSFER" "d['status'] == 'settled'"

say "virtual card: issue, freeze, unfreeze, limits, number display, a purchase"
CARD=$(call POST /cards "{\"account_id\":\"$EUR\"}" | json "d['id']")
call POST "/cards/$CARD/freeze" | json "d['status']"
call POST "/cards/$CARD/unfreeze" | json "d['status']"
call POST "/cards/$CARD/limits" '{"daily":{"amount":"200.00","currency":"EUR"},"online":true}' >/dev/null
PAN=$(call POST "/cards/$CARD/pan-token")
if [ "$(echo "$PAN" | json "d.get('challenge') is not None")" = True ]; then
  call POST "/sca/$(echo "$PAN" | json "d['challenge']['challenge_id']")" "{\"code\":\"$PROVIDER_CODE\"}" >/dev/null
  PAN=$(call POST "/cards/$CARD/pan-token")
fi
echo "$PAN" | json "d.get('expires_at') is not None"
call POST /sandbox/card-auth "{\"card_id\":\"$CARD\",\"merchant\":\"Corner Shop\",\"mcc\":5411,
  \"country\":\"DE\",\"amount\":{\"amount\":\"12.50\",\"currency\":\"EUR\"}}" | json "d['response_code']"

say "monthly standing order of 10.00 EUR"
ORDER=$(call POST /standing-orders "{\"account_id\":\"$EUR\",\"beneficiary_id\":\"$PAYEE\",
  \"amount\":{\"amount\":\"10.00\",\"currency\":\"EUR\"},\"schedule\":\"monthly:1\",
  \"start\":\"$(date -u +%Y-%m-%d)\",\"reference\":\"Rent\"}")
if [ "$(echo "$ORDER" | json "d.get('provider_sca') is not None")" = True ]; then
  call POST "/sca/$(echo "$ORDER" | json "d['provider_sca']['challenge_id']")" "{\"code\":\"$PROVIDER_CODE\"}" >/dev/null
fi
call GET /standing-orders | json "d[0]['status']"

say "statement"
call GET "/accounts/$EUR/statement" | json "d['closing']"
curl -sSf "$API/accounts/$EUR/statement?format=csv" -H "authorization: Bearer $TOKEN" | head -5

if [ -n "$OPS_TOKEN" ]; then
  say "a 600.00 EUR payment is held for review, released by two operators, then confirmed"
  HELD=$(call POST /transfers "{\"account_id\":\"$EUR\",\"beneficiary_id\":\"$PAYEE\",
    \"amount\":{\"amount\":\"600.00\",\"currency\":\"EUR\"}}")
  echo "$HELD" | json "d['status']"
  HELD=$(echo "$HELD" | json "d['id']")
  REVIEW=$(ops GET /ops/review | json "[r['id'] for r in d if r['reference']=='$HELD'][0]")
  # Releasing a held payment takes two different operators.
  ops POST "/ops/review/$REVIEW" '{"verdict":"approve","operator":"operator-1"}' | json "d['status']"
  ops POST "/ops/review/$REVIEW" '{"verdict":"approve","operator":"operator-2"}' | json "d['status']"
  STATE=$(call POST "/transfers/$HELD/confirm-sca" "{\"password\":\"$PASSWORD\",\"code\":\"$(totp "$SECRET" 1)\"}")
  if [ "$(echo "$STATE" | json "d.get('provider_sca') is not None")" = True ]; then
    call POST "/transfers/$HELD/confirm-sca" "{\"code\":\"$PROVIDER_CODE\"}" >/dev/null
  fi
  poll "/transfers/$HELD" "d['status'] == 'settled'"
  ops GET /ops/audit | json "d['chain']"
fi

say "done"
call GET /me | json "json.dumps(d)"
