open! Core
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

module Amount = struct
  type t =
    { amount : string
    ; currency : string
    }
  [@@deriving sexp, compare, equal, yojson] [@@yojson.allow_extra_fields]
end

module Api_error = struct
  type t =
    { error : string
    ; message : string
    }
  [@@deriving sexp, compare, equal, yojson] [@@yojson.allow_extra_fields]
end

module Sca_info = struct
  type t =
    { challenge_id : string
    ; factors : string list
    ; expires_at : string
    ; attempts_left : int
    }
  [@@deriving sexp, compare, equal, yojson] [@@yojson.allow_extra_fields]
end

module Sca_required = struct
  type t =
    { error : string
    ; message : string
    ; challenge : Sca_info.t
    }
  [@@deriving sexp, compare, equal, yojson] [@@yojson.allow_extra_fields]
end

module Auth = struct
  module Address = struct
    type t =
      { line1 : string
      ; line2 : string option [@default None] [@yojson_drop_if Option.is_none]
      ; city : string
      ; postal_code : string option [@default None] [@yojson_drop_if Option.is_none]
      ; country : string
      }
    [@@deriving sexp, compare, equal, yojson] [@@yojson.allow_extra_fields]
  end

  module Signup_request = struct
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
    [@@deriving sexp, compare, equal, yojson] [@@yojson.allow_extra_fields]
  end

  module Signup_response = struct
    type t =
      { customer_id : string
      ; totp_secret : string
      ; otpauth_uri : string
      }
    [@@deriving sexp, compare, equal, yojson] [@@yojson.allow_extra_fields]
  end

  module Login_request = struct
    type t =
      { email : string
      ; password : string
      ; totp : string
      }
    [@@deriving sexp, compare, equal, yojson] [@@yojson.allow_extra_fields]
  end

  module Step_up_request = struct
    type t =
      { password : string
      ; totp : string
      }
    [@@deriving sexp, compare, equal, yojson] [@@yojson.allow_extra_fields]
  end

  module Session = struct
    type t =
      { token : string
      ; customer_id : string
      ; idle_timeout_s : int
      ; sca_at : string
      }
    [@@deriving sexp, compare, equal, yojson] [@@yojson.allow_extra_fields]
  end
end

module Kyc = struct
  module Start_request = struct
    type t = { step : string }
    [@@deriving sexp, compare, equal, yojson] [@@yojson.allow_extra_fields]
  end

  module Session = struct
    type t =
      { step : string
      ; kind : string
      ; sdk_token : string option [@default None] [@yojson_drop_if Option.is_none]
      ; expires_at : string
      }
    [@@deriving sexp, compare, equal, yojson] [@@yojson.allow_extra_fields]
  end

  module Complete_request = struct
    type t =
      { step : string
      ; code : string
      }
    [@@deriving sexp, compare, equal, yojson] [@@yojson.allow_extra_fields]
  end

  module Status = struct
    type t =
      { status : string
      ; tier : string
      ; steps_to_complete : string list
      ; fields_to_resubmit : string list
      }
    [@@deriving sexp, compare, equal, yojson] [@@yojson.allow_extra_fields]
  end
end

module Me = struct
  type t =
    { customer_id : string
    ; email : string
    ; kyc : Kyc.Status.t
    ; accounts : int
    ; sca_fresh : bool
    }
  [@@deriving sexp, compare, equal, yojson] [@@yojson.allow_extra_fields]
end

module Balance = struct
  type t =
    { available : Amount.t
    ; held : Amount.t
    ; posted : Amount.t
    ; as_of : string
    }
  [@@deriving sexp, compare, equal, yojson] [@@yojson.allow_extra_fields]
end

module Account = struct
  type t =
    { id : string
    ; currency : string
    ; status : string
    ; iban : string option [@default None] [@yojson_drop_if Option.is_none]
    ; bic : string option [@default None] [@yojson_drop_if Option.is_none]
    ; address : string option [@default None] [@yojson_drop_if Option.is_none]
    ; network : string option [@default None] [@yojson_drop_if Option.is_none]
    ; lightning_node : string option [@default None] [@yojson_drop_if Option.is_none]
    ; balance : Balance.t option [@default None] [@yojson_drop_if Option.is_none]
    }
  [@@deriving sexp, compare, equal, yojson] [@@yojson.allow_extra_fields]
end

module Statement = struct
  module Line = struct
    type t =
      { at : string
      ; amount : Amount.t
      ; code : string
      ; reference : string
      ; balance : Amount.t
      }
    [@@deriving sexp, compare, equal, yojson] [@@yojson.allow_extra_fields]
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
  [@@deriving sexp, compare, equal, yojson] [@@yojson.allow_extra_fields]
end

module Beneficiary = struct
  module Request = struct
    type t =
      { name : string
      ; rail : string
      ; destination : string
      ; bic : string option [@default None] [@yojson_drop_if Option.is_none]
      ; memo : string option [@default None] [@yojson_drop_if Option.is_none]
      }
    [@@deriving sexp, compare, equal, yojson] [@@yojson.allow_extra_fields]
  end

  type t =
    { id : string
    ; name : string
    ; rails : string list
    ; destination : string
    ; trusted : bool
    ; payee_check : string option [@default None] [@yojson_drop_if Option.is_none]
    ; created_at : string
    }
  [@@deriving sexp, compare, equal, yojson] [@@yojson.allow_extra_fields]
end

module Fee_quote = struct
  module Request = struct
    type t =
      { account_id : string
      ; rail : string
      ; amount : Amount.t
      }
    [@@deriving sexp, compare, equal, yojson] [@@yojson.allow_extra_fields]
  end

  type t =
    { rail : string
    ; amount : Amount.t
    ; fee : Amount.t
    ; total : Amount.t
    }
  [@@deriving sexp, compare, equal, yojson] [@@yojson.allow_extra_fields]
end

module Transfer = struct
  module Request = struct
    type t =
      { account_id : string
      ; beneficiary_id : string
      ; amount : Amount.t
      ; rail : string option [@default None] [@yojson_drop_if Option.is_none]
      ; reference : string option [@default None] [@yojson_drop_if Option.is_none]
      }
    [@@deriving sexp, compare, equal, yojson] [@@yojson.allow_extra_fields]
  end

  module Confirm_request = struct
    type t =
      { code : string
      ; password : string option [@default None] [@yojson_drop_if Option.is_none]
      }
    [@@deriving sexp, compare, equal, yojson] [@@yojson.allow_extra_fields]
  end

  type t =
    { id : string
    ; account_id : string
    ; beneficiary_id : string option [@default None] [@yojson_drop_if Option.is_none]
    ; counterparty : string
    ; direction : string
    ; rail : string
    ; amount : Amount.t
    ; fee : Amount.t
    ; reference : string
    ; status : string
    ; under_review : bool
    ; sca : Sca_info.t option [@default None] [@yojson_drop_if Option.is_none]
    ; provider_sca : Sca_info.t option [@default None] [@yojson_drop_if Option.is_none]
    ; message : string option [@default None] [@yojson_drop_if Option.is_none]
    ; created_at : string
    ; updated_at : string
    }
  [@@deriving sexp, compare, equal, yojson] [@@yojson.allow_extra_fields]
end

module Swap = struct
  module Quote_request = struct
    type t =
      { from_account_id : string
      ; to_account_id : string
      ; sell : Amount.t
      }
    [@@deriving sexp, compare, equal, yojson] [@@yojson.allow_extra_fields]
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
    ; provider_sca : Sca_info.t option [@default None] [@yojson_drop_if Option.is_none]
    }
  [@@deriving sexp, compare, equal, yojson] [@@yojson.allow_extra_fields]
end

module Card = struct
  module Limits = struct
    type t =
      { per_transaction : Amount.t option [@default None] [@yojson_drop_if Option.is_none]
      ; daily : Amount.t option [@default None] [@yojson_drop_if Option.is_none]
      ; monthly : Amount.t option [@default None] [@yojson_drop_if Option.is_none]
      ; online : bool [@default true]
      ; contactless : bool [@default true]
      ; atm : bool [@default true]
      }
    [@@deriving sexp, compare, equal, yojson] [@@yojson.allow_extra_fields]
  end

  module Issue_request = struct
    type t = { account_id : string }
    [@@deriving sexp, compare, equal, yojson] [@@yojson.allow_extra_fields]
  end

  module Pan_token = struct
    type t =
      { token : string
      ; expires_at : string
      }
    [@@deriving sexp, compare, equal, yojson] [@@yojson.allow_extra_fields]
  end

  type t =
    { id : string
    ; account_id : string
    ; kind : string
    ; status : string
    ; masked_pan : string option [@default None] [@yojson_drop_if Option.is_none]
    ; limits : Limits.t
    ; created_at : string
    }
  [@@deriving sexp, compare, equal, yojson] [@@yojson.allow_extra_fields]
end

module Standing_order = struct
  module Request = struct
    type t =
      { account_id : string
      ; beneficiary_id : string
      ; amount : Amount.t
      ; schedule : string
      ; start : string
      ; until : string option [@default None] [@yojson_drop_if Option.is_none]
      ; reference : string option [@default None] [@yojson_drop_if Option.is_none]
      }
    [@@deriving sexp, compare, equal, yojson] [@@yojson.allow_extra_fields]
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
    ; provider_sca : Sca_info.t option [@default None] [@yojson_drop_if Option.is_none]
    }
  [@@deriving sexp, compare, equal, yojson] [@@yojson.allow_extra_fields]
end

module Sca_confirm_request = struct
  type t = { code : string }
  [@@deriving sexp, compare, equal, yojson] [@@yojson.allow_extra_fields]
end

module Review = struct
  module Decide_request = struct
    type t =
      { verdict : string
      ; operator : string
      ; note : string option [@default None] [@yojson_drop_if Option.is_none]
      }
    [@@deriving sexp, compare, equal, yojson] [@@yojson.allow_extra_fields]
  end

  type t =
    { id : string
    ; reference : string
    ; customer_id : string
    ; severity : string
    ; reasons : string list
    ; submitted_at : string
    ; status : string
    ; operator : string option [@default None] [@yojson_drop_if Option.is_none]
    }
  [@@deriving sexp, compare, equal, yojson] [@@yojson.allow_extra_fields]
end

module Ws_push = struct
  type t =
    | Transfer of Transfer.t
    | Account of Account.t
    | Kyc of Kyc.Status.t
    | Card of Card.t
    | Swap of Swap.t
    | Standing_order of Standing_order.t
  [@@deriving sexp, compare, equal]

  let to_yojson t : Yojson.Safe.t =
    let tagged kind json = `Assoc [ "type", `String kind; "data", json ] in
    match t with
    | Transfer x -> tagged "transfer" (Transfer.yojson_of_t x)
    | Account x -> tagged "account" (Account.yojson_of_t x)
    | Kyc x -> tagged "kyc" (Kyc.Status.yojson_of_t x)
    | Card x -> tagged "card" (Card.yojson_of_t x)
    | Swap x -> tagged "swap" (Swap.yojson_of_t x)
    | Standing_order x -> tagged "standing_order" (Standing_order.yojson_of_t x)
  ;;

  let of_yojson (json : Yojson.Safe.t) =
    match json with
    | `Assoc fields ->
      (match
         ( List.Assoc.find fields ~equal:String.equal "type"
         , List.Assoc.find fields ~equal:String.equal "data" )
       with
       | Some (`String kind), Some data ->
         Or_error.try_with (fun () ->
           match kind with
           | "transfer" -> Transfer (Transfer.t_of_yojson data)
           | "account" -> Account (Account.t_of_yojson data)
           | "kyc" -> Kyc (Kyc.Status.t_of_yojson data)
           | "card" -> Card (Card.t_of_yojson data)
           | "swap" -> Swap (Swap.t_of_yojson data)
           | "standing_order" -> Standing_order (Standing_order.t_of_yojson data)
           | other -> failwithf "unknown push type %s" other ())
       | _ -> Or_error.error_string "push without type and data")
    | _ -> Or_error.error_string "push is not an object"
  ;;

  let to_string t = Yojson.Safe.to_string (to_yojson t)
end
