open! Core
module Customer_id = Kyc_core.Customer_id
module Utc_time = Payments_core.Utc_time
module Account_ref = Banking_provider.Account_ref
module Customer_ref = Banking_provider.Customer_ref

module Secret = struct
  module Stable = struct
    module V1 = struct
      type t = string [@@deriving bin_io, compare, equal]

      let sexp_of_t (_ : t) = Sexp.Atom "<redacted>"
      let t_of_sexp = String.t_of_sexp
    end
  end

  include Stable.V1

  let of_string = Fn.id
  let reveal = Fn.id
end

module Provider_challenge = struct
  module Stable = struct
    module V1 = struct
      type t =
        { id : Payments_core.Sca.Id.Stable.V1.t
        ; factors : Payments_core.Sca.Kind.t list
        ; expires_at : Utc_time.Stable.V1.t
        ; attempts_left : int
        }
      [@@deriving bin_io, sexp, compare, equal]
    end
  end

  include Stable.V1

  let of_provider (c : Banking_provider.Sca_challenge.t) =
    { id = c.id
    ; factors = c.factors
    ; expires_at = c.expires_at
    ; attempts_left = c.attempts_left
    }
  ;;
end

module Customer = struct
  module Stable = struct
    module V1 = struct
      type t =
        { id : Customer_id.Stable.V1.t
        ; email : string
        ; password_hash : Secret.Stable.V1.t
        ; totp_secret : Secret.Stable.V1.t
        ; totp_confirmed : bool
        ; last_totp_step : int option
        ; provider_ref : Customer_ref.Stable.V1.t option
        ; sca_lock : Payments_core.Sca.Lock.t
        ; failed_sca_at : Utc_time.Stable.V1.t list
        ; created_at : Utc_time.Stable.V1.t
        }
      [@@deriving bin_io, sexp, compare, equal]
    end
  end

  include Stable.V1

  let normalize_email s = String.lowercase (String.strip s)
end

module Session = struct
  module Stable = struct
    module V1 = struct
      type t =
        { token_hash : string
        ; customer : Customer_id.Stable.V1.t
        ; created_at : Utc_time.Stable.V1.t
        ; last_activity : Utc_time.Stable.V1.t
        ; sca_at : Utc_time.Stable.V1.t
        }
      [@@deriving bin_io, sexp, compare, equal]
    end
  end

  include Stable.V1

  let hash_token token =
    Cryptokit.transform_string
      (Cryptokit.Hexa.encode ())
      (Cryptokit.hash_string (Cryptokit.Hash.sha256 ()) token)
  ;;
end

module Account = struct
  module Stable = struct
    module V1 = struct
      type t =
        { customer : Customer_id.Stable.V1.t
        ; provider : Banking_provider.Account.t
        ; updated_at : Utc_time.Stable.V1.t
        }
      [@@deriving bin_io, sexp, compare, equal]
    end
  end

  include Stable.V1

  let id t = t.provider.id
  let currency t = t.provider.currency
end

module Hold = struct
  module Stable = struct
    module V1 = struct
      type t =
        | Not_held
        | Under_review of { review : string }
        | Released of { review : string }
        | Refused of { review : string }
      [@@deriving bin_io, sexp, compare, equal]
    end
  end

  include Stable.V1
end

module Risk_record = struct
  module Stable = struct
    module V1 = struct
      type t =
        { context : Risk_rules.Context.Stable.V1.t
        ; decision : Risk_rules.Decision.Stable.V1.t
        }
      [@@deriving bin_io, sexp, compare, equal]
    end
  end

  include Stable.V1
end

module Transfer = struct
  module Stable = struct
    module V1 = struct
      type t =
        { transfer : Payments_core.Transfer.Stable.V1.t
        ; customer : Customer_id.Stable.V1.t
        ; account : Account_ref.Stable.V1.t
        ; beneficiary : Payments_core.Beneficiary.Id.Stable.V1.t option
        ; risk : Risk_record.t option
        ; hold : Hold.t
        ; challenge : Payments_core.Sca.Challenge.Stable.V1.t option
        ; provider_challenge : Provider_challenge.t option
        ; created_at : Utc_time.Stable.V1.t
        ; updated_at : Utc_time.Stable.V1.t
        }
      [@@deriving bin_io, sexp, compare, equal]
    end
  end

  include Stable.V1

  let id t = t.transfer.id
end

module Swap_status = struct
  module Stable = struct
    module V1 = struct
      type t =
        | Quoted
        | Executing
        | Awaiting_sca
        | Executed
        | Failed of string
      [@@deriving bin_io, sexp, compare, equal]
    end
  end

  include Stable.V1
end

module Swap = struct
  module Stable = struct
    module V1 = struct
      type t =
        { id : Payments_core.Swap.Id.Stable.V1.t
        ; customer : Customer_id.Stable.V1.t
        ; from_account : Account_ref.Stable.V1.t
        ; to_account : Account_ref.Stable.V1.t
        ; sell : Money.Amount.Stable.V1.t
        ; fee : Money.Amount.Stable.V1.t
        ; buy : Money.Amount.Stable.V1.t
        ; price : string (** what the customer gets per unit sold, exact *)
        ; expires_at : Utc_time.Stable.V1.t
        ; status : Swap_status.t
        ; provider_challenge : Provider_challenge.t option
        ; created_at : Utc_time.Stable.V1.t
        ; updated_at : Utc_time.Stable.V1.t
        }
      [@@deriving bin_io, sexp, compare, equal]
    end
  end

  include Stable.V1
end

module Card = struct
  module Stable = struct
    module V1 = struct
      type t =
        { id : Card_core.Ids.Card_id.Stable.V1.t
        ; customer : Customer_id.Stable.V1.t
        ; account : Account_ref.Stable.V1.t
        ; request_id : string
        ; kind : Card_core.Card.Kind.Stable.V1.t
        ; status : Card_core.Card.Status.Stable.V1.t
        ; masked_pan : Card_core.Masked_pan.Stable.V1.t option
        ; limits : Card_core.Limits.Stable.V1.t
        ; created_at : Utc_time.Stable.V1.t
        ; updated_at : Utc_time.Stable.V1.t
        }
      [@@deriving bin_io, sexp, compare, equal]
    end
  end

  include Stable.V1

  let of_info (info : Banking_provider.Card_info.t) ~customer ~request_id ~now =
    { id = info.id
    ; customer
    ; account = info.account
    ; request_id
    ; kind = info.kind
    ; status = info.status
    ; masked_pan = info.masked_pan
    ; limits = info.limits
    ; created_at = info.created_at
    ; updated_at = now
    }
  ;;
end

module Standing_order = struct
  module Stable = struct
    module V1 = struct
      type t =
        { order : Payments_core.Standing_order.Stable.V1.t
        ; customer : Customer_id.Stable.V1.t
        ; account : Account_ref.Stable.V1.t
        ; provider_ref : Banking_provider.Standing_order_ref.Stable.V1.t option
        ; provider_challenge : Provider_challenge.t option
        ; created_at : Utc_time.Stable.V1.t
        ; updated_at : Utc_time.Stable.V1.t
        }
      [@@deriving bin_io, sexp, compare, equal]
    end
  end

  include Stable.V1

  let id t = t.order.id
end

module Review = struct
  module Status = struct
    module Stable = struct
      module V1 = struct
        type t =
          | Pending
          | Approved of
              { operator : string
              ; at : Utc_time.Stable.V1.t
              }
          | Rejected of
              { operator : string
              ; at : Utc_time.Stable.V1.t
              }
          | First_approval of
              { operator : string
              ; at : Utc_time.Stable.V1.t
              }
          (** releasing a held payment takes two different operators (four eyes); a single
              one may reject *)
        [@@deriving bin_io, sexp, compare, equal]
      end
    end

    include Stable.V1
  end

  module Stable = struct
    module V1 = struct
      type t =
        { id : string
        ; reference : string (** the held transfer *)
        ; customer : Customer_id.Stable.V1.t
        ; risk : Risk_record.t
        ; submitted_at : Utc_time.Stable.V1.t
        ; status : Status.t
        ; notes : string list
        }
      [@@deriving bin_io, sexp, compare, equal]
    end
  end

  include Stable.V1

  let is_pending t =
    match t.status with
    | Pending | First_approval _ -> true
    | Approved _ | Rejected _ -> false
  ;;
end

module Outbox = struct
  module Kind = struct
    module Stable = struct
      module V1 = struct
        type t =
          | Create_customer of Customer_id.Stable.V1.t
          | Open_accounts of Customer_id.Stable.V1.t
          | Initiate_transfer of Payments_core.Transfer.Id.Stable.V1.t
          | Execute_swap of Payments_core.Swap.Id.Stable.V1.t
          | Create_standing_order of Payments_core.Standing_order.Id.Stable.V1.t
        [@@deriving bin_io, sexp, compare, equal]
      end
    end

    include Stable.V1

    let name = function
      | Create_customer _ -> "create_customer"
      | Open_accounts _ -> "open_accounts"
      | Initiate_transfer _ -> "initiate_transfer"
      | Execute_swap _ -> "execute_swap"
      | Create_standing_order _ -> "create_standing_order"
    ;;

    let aggregate_id = function
      | Create_customer id | Open_accounts id -> Customer_id.to_string id
      | Initiate_transfer id -> Payments_core.Transfer.Id.to_string id
      | Execute_swap id -> Payments_core.Swap.Id.to_string id
      | Create_standing_order id -> Payments_core.Standing_order.Id.to_string id
    ;;
  end

  module State = struct
    module Stable = struct
      module V1 = struct
        type t =
          | Pending
          | Sent of { at : Utc_time.Stable.V1.t }
          | Needs_review (** gave up after the retry window; an operator looks at it *)
        [@@deriving bin_io, sexp, compare, equal]
      end
    end

    include Stable.V1

    let name = function
      | Pending -> "pending"
      | Sent _ -> "sent"
      | Needs_review -> "needs_review"
    ;;
  end

  module Stable = struct
    module V1 = struct
      type t =
        { id : string
        ; kind : Kind.t
        ; attempts : int
        ; next_attempt_at : Utc_time.Stable.V1.t
        ; created_at : Utc_time.Stable.V1.t
        ; state : State.t
        ; last_error : string option
        }
      [@@deriving bin_io, sexp, compare, equal]
    end
  end

  include Stable.V1

  let create ~id ~now kind =
    { id
    ; kind
    ; attempts = 0
    ; next_attempt_at = now
    ; created_at = now
    ; state = Pending
    ; last_error = None
    }
  ;;
end

module Inbox_entry = struct
  module Stable = struct
    module V1 = struct
      type t =
        { event_id : Banking_provider.Event_id.Stable.V1.t
        ; received_at : Utc_time.Stable.V1.t
        ; payload_hash : string
        }
      [@@deriving bin_io, sexp, compare, equal]
    end
  end

  include Stable.V1

  let of_event (e : Banking_provider.Event.t) ~received_at =
    let bytes =
      Bin_prot.Writer.to_string Banking_provider.Event.Stable.V1.bin_writer_t e
    in
    { event_id = e.id
    ; received_at
    ; payload_hash =
        Cryptokit.transform_string
          (Cryptokit.Hexa.encode ())
          (Cryptokit.hash_string (Cryptokit.Hash.sha256 ()) bytes)
    }
  ;;
end

module Ledger_posting = struct
  type t =
    { seq : int
    ; line : int
    ; entry_key : string
    ; account : string
    ; side : Ledger.Side.t
    ; amount : Money.Amount.t
    ; code : string
    ; reference : string
    ; recorded_at : Utc_time.t
    }
  [@@deriving sexp, compare, equal]

  let of_record (r : Ledger.Record.t) =
    List.concat_map (Ledger.Record.entries r) ~f:(fun (e : Ledger.Entry.t) ->
      List.map e.postings ~f:(fun (p : Ledger.Posting.t) ->
        ( Ledger.Idempotency_key.to_string e.idempotency_key
        , Ledger.Entry.Code.to_string e.code
        , e.reference
        , e.recorded_at
        , p )))
    |> List.mapi
         ~f:(fun line (entry_key, code, reference, recorded_at, (p : Ledger.Posting.t)) ->
           { seq = r.seq
           ; line
           ; entry_key
           ; account = Ledger.Account_id.to_string p.account
           ; side = p.side
           ; amount = p.amount
           ; code
           ; reference
           ; recorded_at
           })
  ;;

  let signed t =
    match t.side with
    | Debit -> t.amount
    | Credit -> Money.Amount.neg t.amount
  ;;
end

module Idempotency = struct
  module Scope = struct
    type t =
      { customer : string
      ; meth : string
      ; route : string
      ; key : string
      }
    [@@deriving sexp, compare, equal]

    let to_string t = String.concat ~sep:" " [ t.customer; t.meth; t.route; t.key ]
  end

  module Begin = struct
    type t =
      | Started
      | Replay of
          { status : int
          ; body : string
          }
      | In_progress
      | Mismatch
    [@@deriving sexp, compare, equal]
  end
end

module Audit = struct
  module Record = struct
    module Stable = struct
      module V1 = struct
        type t =
          { actor : string
          ; on_behalf_of : string option
          ; action : string
          ; object_id : string
          ; at : Utc_time.Stable.V1.t
          ; source : string
          ; outcome : string
          }
        [@@deriving bin_io, sexp, compare, equal]
      end
    end

    include Stable.V1
  end

  type t =
    { seq : int
    ; record : Record.t
    ; prev_hash : string
    ; hash : string
    }
  [@@deriving sexp, compare, equal]

  let genesis_hash = String.make 64 '0'

  let chain_hash ~prev_hash record =
    let bytes = Bin_prot.Writer.to_string Record.Stable.V1.bin_writer_t record in
    Cryptokit.transform_string
      (Cryptokit.Hexa.encode ())
      (Cryptokit.hash_string (Cryptokit.Hash.sha256 ()) (prev_hash ^ bytes))
  ;;

  let verify_chain ts =
    List.fold_result ts ~init:genesis_hash ~f:(fun prev t ->
      if String.equal t.prev_hash prev
         && String.equal t.hash (chain_hash ~prev_hash:prev t.record)
      then Ok t.hash
      else Or_error.error_s [%message "audit chain broken" ~seq:(t.seq : int)])
    |> Or_error.ignore_m
  ;;
end
