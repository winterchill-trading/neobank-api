open! Core

type t = T : (module Store.S with type t = 'a) * 'a -> t

let kind = "any"
let pack (type a) (module S : Store.S with type t = a) (s : a) = T ((module S), s)
let kind_of (T ((module S), _)) = S.kind
let commit (T ((module S), s)) = S.commit s
let customer (T ((module S), s)) = S.customer s
let customer_by_email (T ((module S), s)) = S.customer_by_email s
let customer_by_provider_ref (T ((module S), s)) = S.customer_by_provider_ref s
let session (T ((module S), s)) = S.session s
let kyc_events (T ((module S), s)) = S.kyc_events s
let account (T ((module S), s)) = S.account s
let accounts (T ((module S), s)) = S.accounts s
let beneficiary (T ((module S), s)) = S.beneficiary s
let beneficiaries (T ((module S), s)) = S.beneficiaries s
let transfer (T ((module S), s)) = S.transfer s
let transfers (T ((module S), s)) = S.transfers s
let swap (T ((module S), s)) = S.swap s
let swaps (T ((module S), s)) = S.swaps s
let card (T ((module S), s)) = S.card s
let card_by_request (T ((module S), s)) = S.card_by_request s
let cards (T ((module S), s)) = S.cards s
let standing_order (T ((module S), s)) = S.standing_order s

let standing_order_by_provider_ref (T ((module S), s)) =
  S.standing_order_by_provider_ref s
;;

let standing_orders (T ((module S), s)) = S.standing_orders s
let review (T ((module S), s)) = S.review s
let reviews (T ((module S), s)) = S.reviews s
let outbox_due (T ((module S), s)) = S.outbox_due s
let outbox (T ((module S), s)) = S.outbox s
let outbox_in_state (T ((module S), s)) = S.outbox_in_state s
let inbox_mem (T ((module S), s)) = S.inbox_mem s
let idempotency_begin (T ((module S), s)) = S.idempotency_begin s
let idempotency_complete (T ((module S), s)) = S.idempotency_complete s
let idempotency_abandon (T ((module S), s)) = S.idempotency_abandon s
let audit_log (T ((module S), s)) = S.audit_log s
let ledger_head (T ((module S), s)) = S.ledger_head s
let ledger_postings (T ((module S), s)) = S.ledger_postings s
let ledger_trial_balance (T ((module S), s)) = S.ledger_trial_balance s
let prune (T ((module S), s)) = S.prune s
