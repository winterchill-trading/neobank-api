open! Core
open Async
open Neobank_api_types
open Model

let ( let* ) = Deferred.Result.( >>= )
let ok = Deferred.Result.return
let fail p = return (Error p)

let confirm (ctx : Ctx.t) customer id (r : Api_types.Sca_confirm_request.t) =
  match Hashtbl.find ctx.provider_challenges id with
  | Some (owner, _) when not (Customer_id.equal owner customer) ->
    fail (Problem.not_found "challenge")
  | None -> fail (Problem.not_found "challenge")
  | Some (_, Transfer transfer) ->
    let* (_ : Api_types.Transfer.t) =
      Payments.confirm_sca
        ctx
        customer
        (Payments_core.Transfer.Id.to_string transfer)
        { code = r.code; password = None }
    in
    Hashtbl.remove ctx.provider_challenges id;
    ok `Accepted
  | Some (_, purpose) ->
    let* result =
      Deferred.map
        (Bank.confirm_sca
           ctx.bank
           (Payments_core.Sca.Id.of_string id)
           ~code:(Banking_provider.Sensitive.of_string r.code))
        ~f:Problem.of_provider
    in
    let* () =
      Ctx.commit
        ctx
        [ Ctx.audit
            ctx
            ~actor:("customer:" ^ Customer_id.to_string customer)
            ~action:"provider_sca.confirm"
            ~object_id:id
            ~outcome:
              (match result with
               | Accepted -> "accepted"
               | Wrong_code _ -> "wrong"
               | Failed _ -> "failed")
            ()
        ]
    in
    (match result with
     | Accepted ->
       Hashtbl.remove ctx.provider_challenges id;
       let* () =
         match purpose with
         | Swap swap -> Swaps.challenge_accepted ctx swap
         | Standing_order r -> Standing_orders.challenge_accepted ctx r
         | Card_details _ | Transfer _ -> ok ()
       in
       let%bind () = ctx.kick () in
       ok `Accepted
     | Wrong_code { attempts_left } ->
       fail
         (Problem.unprocessable
            "sca_failed"
            (sprintf "the code is wrong; %d attempts left" attempts_left))
     | Failed _ ->
       Hashtbl.remove ctx.provider_challenges id;
       fail
         (Problem.unprocessable
            "sca_failed"
            "the challenge failed; start the operation again"))
;;
