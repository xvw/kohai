type t =
  | Parse_error
  | Invalid_request of int option * Rensai.Validation.value_error option
  | Method_not_found of int option * string option
  | Invalid_params of int option * Rensai.Validation.value_error option
  | Internal_error of int option
  | Custom_error of int option * int * string

let as_value_error = Option.map Rensai.Validation.value_error_ast
let as_string = Option.map Rensai.Ast.string

let to_rensai error =
  let res ?data id code message =
    let open Rensai.Ast in
    record
      [ "jsonrpc", string "2.0"
      ; "id", option int id
      ; ( "error"
        , record
            [ "code", int (0 - abs code)
            ; "message", string message
            ; "data", option Fun.id data
            ] )
      ]
  in
  match error with
  | Parse_error -> res None 32700 "Parse error"
  | Invalid_request (id, data) ->
    res ?data:(as_value_error data) id 32600 "Invalid Request"
  | Method_not_found (id, data) ->
    res ?data:(as_string data) id 32601 "Method not found"
  | Invalid_params (id, data) ->
    res ?data:(as_value_error data) id 32602 "Invalid params"
  | Internal_error id -> res id 32603 "Internal error"
  | Custom_error (id, code, message) -> res id code message
;;

let parse_error () = Parse_error
let invalid_request ?data ?id () = Invalid_request (id, data)
let method_not_found ?data ?id () = Method_not_found (id, data)
let invalid_params ?data ?id () = Invalid_params (id, data)
let internal_error ?id () = Internal_error id
let custom_error ?id ~code ~message () = Custom_error (id, code, message)

let no_supervised_directory ?id () =
  custom_error ?id ~code:32000 ~message:"No supervised directory" ()
;;
