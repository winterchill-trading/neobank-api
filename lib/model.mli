(** Records the API keeps in its store, in domain types.

    Each is a [Stable.V1] Bin_prot type: {!Store_pg} keeps the Bin_prot bytes in a [body]
    column next to the columns queries filter on, and the memory store keeps the values.
    Personal data stays in kyc-core's [Pii] wrappers (in the KYC events), and secrets in
    {!Secret}, so printing any record never shows them. *)

open! Core
module Customer_id = Kyc_core.Customer_id
module Utc_time = Payments_core.Utc_time
module Account_ref = Banking_provider.Account_ref
module Customer_ref = Banking_provider.Customer_ref

(** A string that prints as [<redacted>] in sexps (password hashes, TOTP secrets).
    Bin_prot carries the clear value, so records persist with it. *)
module Secret : sig
  module Stable : sig
    module V1 : sig
      type t [@@deriving bin_io, sexp, compare, equal]
    end
  end

  type t = Stable.V1.t [@@deriving bin_io, sexp, compare, equal]

  val of_string : string -> t
  val reveal : t -> string
end

module Provider_challenge : sig
  module Stable : sig
    module V1 : sig
      type t =
        { id : Payments_core.Sca.Id.Stable.V1.t
        ; factors : Payments_core.Sca.Kind.t list
        ; expires_at : Utc_time.Stable.V1.t
        ; attempts_left : int
        }
      [@@deriving bin_io, sexp, compare, equal]
    end
  end

  include module type of struct
    include Stable.V1
  end

  val of_provider : Banking_provider.Sca_challenge.t -> t
end

module Customer : sig
  module Stable : sig
    module V1 : sig
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

  include module type of struct
    include Stable.V1
  end

  (** Lower case, surrounding spaces removed: the form stored and looked up. *)
  val normalize_email : string -> string
end

module Session : sig
  module Stable : sig
    module V1 : sig
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

  include module type of struct
    include Stable.V1
  end

  (** Hex SHA-256 of a bearer token: the store keeps this, never the token. *)
  val hash_token : string -> string
end

module Account : sig
  module Stable : sig
    module V1 : sig
      type t =
        { customer : Customer_id.Stable.V1.t
        ; provider : Banking_provider.Account.t
        ; updated_at : Utc_time.Stable.V1.t
        }
      [@@deriving bin_io, sexp, compare, equal]
    end
  end

  include module type of struct
    include Stable.V1
  end

  val id : t -> Account_ref.t
  val currency : t -> Money.Currency.t
end

module Hold : sig
  module Stable : sig
    module V1 : sig
      type t =
        | Not_held
        | Under_review of { review : string }
        | Released of { review : string }
        | Refused of { review : string }
      [@@deriving bin_io, sexp, compare, equal]
    end
  end

  include module type of struct
    include Stable.V1
  end
end

module Risk_record : sig
  module Stable : sig
    module V1 : sig
      type t =
        { context : Risk_rules.Context.Stable.V1.t
        ; decision : Risk_rules.Decision.Stable.V1.t
        }
      [@@deriving bin_io, sexp, compare, equal]
    end
  end

  include module type of struct
    include Stable.V1
  end
end

module Transfer : sig
  module Stable : sig
    module V1 : sig
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

  include module type of struct
    include Stable.V1
  end

  val id : t -> Payments_core.Transfer.Id.t
end

module Swap_status : sig
  module Stable : sig
    module V1 : sig
      type t =
        | Quoted
        | Executing
        | Awaiting_sca
        | Executed
        | Failed of string
      [@@deriving bin_io, sexp, compare, equal]
    end
  end

  include module type of struct
    include Stable.V1
  end
end

module Swap : sig
  module Stable : sig
    module V1 : sig
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

  include module type of struct
    include Stable.V1
  end
end

module Card : sig
  module Stable : sig
    module V1 : sig
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

  include module type of struct
    include Stable.V1
  end

  val of_info
    :  Banking_provider.Card_info.t
    -> customer:Customer_id.t
    -> request_id:string
    -> now:Time_ns.t
    -> t
end

module Standing_order : sig
  module Stable : sig
    module V1 : sig
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

  include module type of struct
    include Stable.V1
  end

  val id : t -> Payments_core.Standing_order.Id.t
end

module Review : sig
  module Status : sig
    module Stable : sig
      module V1 : sig
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
        [@@deriving bin_io, sexp, compare, equal]
      end
    end

    include module type of struct
      include Stable.V1
    end
  end

  module Stable : sig
    module V1 : sig
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

  include module type of struct
    include Stable.V1
  end

  val is_pending : t -> bool
end

module Outbox : sig
  module Kind : sig
    module Stable : sig
      module V1 : sig
        type t =
          | Create_customer of Customer_id.Stable.V1.t
          | Open_accounts of Customer_id.Stable.V1.t
          | Initiate_transfer of Payments_core.Transfer.Id.Stable.V1.t
          | Execute_swap of Payments_core.Swap.Id.Stable.V1.t
          | Create_standing_order of Payments_core.Standing_order.Id.Stable.V1.t
        [@@deriving bin_io, sexp, compare, equal]
      end
    end

    include module type of struct
      include Stable.V1
    end

    val name : t -> string
    val aggregate_id : t -> string
  end

  module State : sig
    module Stable : sig
      module V1 : sig
        type t =
          | Pending
          | Sent of { at : Utc_time.Stable.V1.t }
          | Needs_review (** gave up after the retry window; an operator looks at it *)
        [@@deriving bin_io, sexp, compare, equal]
      end
    end

    include module type of struct
      include Stable.V1
    end

    val name : t -> string
  end

  module Stable : sig
    module V1 : sig
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

  include module type of struct
    include Stable.V1
  end

  (** A pending message due at [now]. *)
  val create : id:string -> now:Time_ns.t -> Kind.t -> t
end

module Inbox_entry : sig
  module Stable : sig
    module V1 : sig
      type t =
        { event_id : Banking_provider.Event_id.Stable.V1.t
        ; received_at : Utc_time.Stable.V1.t
        ; payload_hash : string
        }
      [@@deriving bin_io, sexp, compare, equal]
    end
  end

  include module type of struct
    include Stable.V1
  end

  (** [payload_hash] is the hex SHA-256 of the event's Bin_prot bytes. *)
  val of_event : Banking_provider.Event.t -> received_at:Time_ns.t -> t
end

(** API idempotency keys: a key is scoped to (customer, method, route, key) and bound to
    the SHA-256 of the canonical request body. *)
module Idempotency : sig
  module Scope : sig
    type t =
      { customer : string
      ; meth : string
      ; route : string
      ; key : string
      }
    [@@deriving sexp, compare, equal]

    val to_string : t -> string
  end

  (** What starting a request under a key found. *)
  module Begin : sig
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

module Audit : sig
  module Record : sig
    module Stable : sig
      module V1 : sig
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

    include module type of struct
      include Stable.V1
    end
  end

  (** A record in the hash chain: [hash] is the hex SHA-256 of [prev_hash] followed by the
      record's Bin_prot bytes, so editing or deleting a record breaks every later link. *)
  type t =
    { seq : int
    ; record : Record.t
    ; prev_hash : string
    ; hash : string
    }
  [@@deriving sexp, compare, equal]

  (** 64 zeros. *)
  val genesis_hash : string

  val chain_hash : prev_hash:string -> Record.t -> string

  (** Checks every link from the genesis hash, in [seq] order. *)
  val verify_chain : t list -> unit Or_error.t
end
