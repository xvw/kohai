type input =
  { meth : string
  ; params : Rensai.Ast.t
  ; id : int option
  }

type handler =
  | Handler :
      'a Rensai.Validation.t
      * ('b -> Rensai.Ast.t)
      * (?id:int -> 'a -> 'b Eff.t)
      -> handler

let handler ~meth ~with_params ~finalizer callback =
  meth, Handler (with_params, finalizer, callback)
;;

let validate_input =
  let open Rensai.Validation in
  record (fun f ->
    let open Record in
    let+ () = ensure f "jsonrpc" (string & String.equal "2.0")
    and+ meth = required f "method" string
    and+ id = optional f "id" int
    and+ params = optional_or ~default:(Rensai.Ast.null ()) f "params" ast in
    { id; meth; params })
;;

let from_response input =
  try
    input
    |> Yojson.Safe.from_string
    |> Rensai.Json.from_yojson
    |> validate_input
    |> Eff.from_validated (fun data -> Error.invalid_request ~data ())
  with
  | _ -> Eff.parse_error ()
;;

let return_success id value =
  let open Rensai.Ast in
  record [ "jsonrpc", string "2.0"; "id", option int id; "result", value ]
;;

let handle input handler =
  let open Core.IO in
  let* { id; meth; params } = from_response input in
  handler meth params id
;;

let eliminate_error p =
  try Ok (p ()) with
  | effect Eff.K_fail_with err, _k -> Error err
;;

let services list input () =
  let open Core.IO in
  let program =
    handle input (fun meth params id ->
      match List.assoc_opt meth list with
      | None -> Eff.method_not_found ?id meth ()
      | Some (Handler (validator, finalizer, handler)) ->
        let* params =
          params
          |> validator
          |> Eff.from_validated (fun data -> Error.invalid_params ~data ?id ())
        in
        let+ result = handler ?id params in
        return_success id (finalizer result))
  in
  eliminate_error program
;;
