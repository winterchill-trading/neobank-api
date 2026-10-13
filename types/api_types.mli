(** Request and response bodies of the neobank API, shared with the web app.

    Conventions: amounts are [{"amount": "12.34", "currency": "EUR"}] with the decimal as
    a string (never a JSON number); times are ISO 8601 UTC strings; dates are
    [YYYY-MM-DD]; ids are opaque strings. Optional fields are omitted when absent, and
    unknown fields are ignored on input. Errors are {!Api_error.t} with a stable [error]
    code and a human [message]. *)

open! Core

module Amount : sig
  type t =
    { amount : string
    ; currency : string
    }
  [@@deriving sexp, compare, equal, yojson]
end

module Api_error : sig
  type t =
    { error : string
    ; message : string
    }
  [@@deriving sexp, compare, equal, yojson]
end

module Sca_info : sig
  type t =
    { challenge_id : string
    ; factors : string list
    ; expires_at : string
    ; attempts_left : int
    }
  [@@deriving sexp, compare, equal, yojson]
end

module Sca_required : sig
  type t =
    { error : string
    ; message : string
    ; challenge : Sca_info.t
    }
  [@@deriving sexp, compare, equal, yojson]
end

module Auth : sig
  module Address : sig
    type t =
      { line1 : string
      ; line2 : string option
      ; city : string
      ; postal_code : string option
      ; country : string
      }
    [@@deriving sexp, compare, equal, yojson]
  end

  module Signup_request : sig
    type t =
      { email : string
      ; password : string
      ; given_name : string
      ; family_name : string
      ; date_of_birth : string
      ; nationality : string
      ; mobile : string
      ; address : Address.t
      }
    [@@deriving sexp, compare, equal, yojson]
  end

  module Signup_response : sig
    type t =
      { customer_id : string
      ; totp_secret : string
      ; otpauth_uri : string
      }
    [@@deriving sexp, compare, equal, yojson]
  end

  module Login_request : sig
    type t =
      { email : string
      ; password : string
      ; totp : string
      }
    [@@deriving sexp, compare, equal, yojson]
  end

  module Step_up_request : sig
    type t =
      { password : string
      ; totp : string
      }
    [@@deriving sexp, compare, equal, yojson]
  end

  module Session : sig
    type t =
      { token : string
      ; customer_id : string
      ; idle_timeout_s : int
      ; sca_at : string
      }
    [@@deriving sexp, compare, equal, yojson]
  end
end

module Kyc : sig
  module Start_request : sig
    type t = { step : string } [@@deriving sexp, compare, equal, yojson]
  end

  module Session : sig
    type t =
      { step : string
      ; kind : string
      ; sdk_token : string option
      ; expires_at : string
      }
    [@@deriving sexp, compare, equal, yojson]
  end

  module Complete_request : sig
    type t =
      { step : string
      ; code : string
      }
    [@@deriving sexp, compare, equal, yojson]
  end

  module Status : sig
    type t =
      { status : string
      ; tier : string
      ; steps_to_complete : string list
      ; fields_to_resubmit : string list
      }
    [@@deriving sexp, compare, equal, yojson]
  end
end

module Me : sig
  type t =
    { customer_id : string
    ; email : string
    ; kyc : Kyc.Status.t
    ; accounts : int
    ; sca_fresh : bool
    }
  [@@deriving sexp, compare, equal, yojson]
end

module Balance : sig
  type t =
    { available : Amount.t
    ; held : Amount.t
    ; posted : Amount.t
    ; as_of : string
    }
  [@@deriving sexp, compare, equal, yojson]
end

module Account : sig
  type t =
    { id : string
    ; currency : string
    ; status : string
    ; iban : string option
    ; bic : string option
    ; address : string option
    ; network : string option
    ; lightning_node : string option
    ; balance : Balance.t option
    }
  [@@deriving sexp, compare, equal, yojson]
end

module Statement : sig
  module Line : sig
    type t =
      { at : string
      ; amount : Amount.t
      ; code : string
      ; reference : string
      ; balance : Amount.t
      }
    [@@deriving sexp, compare, equal, yojson]
  end

  type t =
    { account_id : string
    ; currency : string
    ; from : string
    ; until : string
    ; opening : Amount.t
    ; closing : Amount.t
    ; lines : Line.t list
    }
  [@@deriving sexp, compare, equal, yojson]
end

module Beneficiary : sig
  module Request : sig
    type t =
      { name : string
      ; rail : string
      ; destination : string
      ; bic : string option
      ; memo : string option
      }
    [@@deriving sexp, compare, equal, yojson]
  end

  type t =
    { id : string
    ; name : string
    ; rails : string list
    ; destination : string
    ; trusted : bool
    ; payee_check : string option
    ; created_at : string
    }
  [@@deriving sexp, compare, equal, yojson]
end

module Fee_quote : sig
  module Request : sig
    type t =
      { account_id : string
      ; rail : string
      ; amount : Amount.t
      }
    [@@deriving sexp, compare, equal, yojson]
  end

  type t =
    { rail : string
    ; amount : Amount.t
    ; fee : Amount.t
    ; total : Amount.t
    }
  [@@deriving sexp, compare, equal, yojson]
end

module Transfer : sig
  module Request : sig
    type t =
      { account_id : string
      ; beneficiary_id : string
      ; amount : Amount.t
      ; rail : string option
      ; reference : string option
      }
    [@@deriving sexp, compare, equal, yojson]
  end

  module Confirm_request : sig
    type t =
      { code : string
      ; password : string option
      }
    [@@deriving sexp, compare, equal, yojson]
  end

  type t =
    { id : string
    ; account_id : string
    ; beneficiary_id : string option
    ; counterparty : string
    ; direction : string
    ; rail : string
    ; amount : Amount.t
    ; fee : Amount.t
    ; reference : string
    ; status : string
    ; under_review : bool
    ; sca : Sca_info.t option
    ; provider_sca : Sca_info.t option
    ; message : string option
    ; created_at : string
    ; updated_at : string
    }
  [@@deriving sexp, compare, equal, yojson]
end

module Swap : sig
  module Quote_request : sig
    type t =
      { from_account_id : string
      ; to_account_id : string
      ; sell : Amount.t
      }
    [@@deriving sexp, compare, equal, yojson]
  end

  type t =
    { id : string
    ; from_account_id : string
    ; to_account_id : string
    ; sell : Amount.t
    ; fee : Amount.t
    ; buy : Amount.t
    ; price : string
    ; expires_at : string
    ; status : string
    ; provider_sca : Sca_info.t option
    }
  [@@deriving sexp, compare, equal, yojson]
end

module Card : sig
  module Limits : sig
    type t =
      { per_transaction : Amount.t option
      ; daily : Amount.t option
      ; monthly : Amount.t option
      ; online : bool
      ; contactless : bool
      ; atm : bool
      }
    [@@deriving sexp, compare, equal, yojson]
  end

  module Issue_request : sig
    type t = { account_id : string } [@@deriving sexp, compare, equal, yojson]
  end

  module Pan_token : sig
    type t =
      { token : string
      ; expires_at : string
      }
    [@@deriving sexp, compare, equal, yojson]
  end

  type t =
    { id : string
    ; account_id : string
    ; kind : string
    ; status : string
    ; masked_pan : string option
    ; limits : Limits.t
    ; created_at : string
    }
  [@@deriving sexp, compare, equal, yojson]
end

module Standing_order : sig
  module Request : sig
    type t =
      { account_id : string
      ; beneficiary_id : string
      ; amount : Amount.t
      ; schedule : string
      ; start : string
      ; until : string option
      ; reference : string option
      }
    [@@deriving sexp, compare, equal, yojson]
  end

  type t =
    { id : string
    ; account_id : string
    ; beneficiary_id : string
    ; amount : Amount.t
    ; schedule : string
    ; next_run : string
    ; status : string
    ; runs : int
    ; provider_sca : Sca_info.t option
    }
  [@@deriving sexp, compare, equal, yojson]
end

module Sca_confirm_request : sig
  type t = { code : string } [@@deriving sexp, compare, equal, yojson]
end

module Review : sig
  module Decide_request : sig
    type t =
      { verdict : string
      ; operator : string
      ; note : string option
      }
    [@@deriving sexp, compare, equal, yojson]
  end

  type t =
    { id : string
    ; reference : string
    ; customer_id : string
    ; severity : string
    ; reasons : string list
    ; submitted_at : string
    ; status : string
    ; operator : string option
    }
  [@@deriving sexp, compare, equal, yojson]
end

module Ws_push : sig
  (** Pushed on [/ws] as [{"type": "<kind>", "data": {...}}]. *)
  type t =
    | Transfer of Transfer.t
    | Account of Account.t
    | Kyc of Kyc.Status.t
    | Card of Card.t
    | Swap of Swap.t
    | Standing_order of Standing_order.t
  [@@deriving sexp, compare, equal]

  val to_yojson : t -> Yojson.Safe.t
  val of_yojson : Yojson.Safe.t -> t Or_error.t
  val to_string : t -> string
end
