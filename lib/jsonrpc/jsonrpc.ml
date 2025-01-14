type input =
  { meth : string
  ; params : Rensai.Ast.t option
  ; id : int option
  }

type error =
  | Parse_error
  | Invalid_request
  | Method_not_found
  | Invalid_params
  | Internal_error
  | Custom_error of int

let error_to_rensai error =
  let res code message =
    let open Rensai.Ast in
    record [ "code", int (0 - abs code); "message", string message ]
  in
  match error with
  | Parse_error -> res 32700 "Parse error"
  | Invalid_request -> res 32600 "Invalid Request"
  | Method_not_found -> res 32601 "Method not found"
  | Invalid_params -> res 32602 "Invalid params"
  | Internal_error -> res 32603 "Internal error"
  | Custom_error code -> res code "Internal error"
;;

let validate_input =
  let open Rensai.Validation in
  record (fun f ->
    let open Record in
    let+ () = ensure f "jsonrpc" (string & String.equal "2.0")
    and+ meth = required f "method" (string & String.is_slug ~separator:'/')
    and+ id = optional f "id" int
    and+ params = optional f "params" ast in
    { id; meth; params })
;;

let from_data input =
  try
    input
    |> Yojson.Safe.from_string
    |> Rensai.Json.from_yojson
    |> validate_input
    |> Result.map_error (Fun.const Invalid_request)
  with
  | _ -> Error Parse_error
;;
