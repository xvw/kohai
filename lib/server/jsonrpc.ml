type input =
  { meth : string
  ; params : Rensai.Ast.t option
  ; id : int option
  }

type error =
  | Parse_error
  | Invalid_request of int option * Rensai.Validation.value_error option
  | Method_not_found of int option
  | Invalid_params of int option * Rensai.Validation.value_error option
  | Internal_error of int option
  | Custom_error of int option * int * string

type handler =
  | Handler :
      (Rensai.Ast.t option -> 'a Rensai.Validation.checked)
      * (?id:int -> 'a -> (Rensai.Ast.t, error) result)
      -> handler

let error_to_rensai error =
  let res ?data id code message =
    let open Rensai.Ast in
    record
      [ "jsonrpc", string "2.0"
      ; "id", option int id
      ; ( "error"
        , record
            [ "code", int (0 - abs code)
            ; "message", string message
            ; "data", option Rensai.Validation.value_error_ast data
            ] )
      ]
  in
  match error with
  | Parse_error -> `Bad_request, res None 32700 "Parse error"
  | Invalid_request (id, data) ->
    `Bad_request, res ?data id 32600 "Invalid Request"
  | Method_not_found id -> `Not_found, res id 32601 "Method not found"
  | Invalid_params (id, data) ->
    `Internal_server_error, res ?data id 32602 "Invalid params"
  | Internal_error id -> `Internal_server_error, res id 32603 "Internal error"
  | Custom_error (id, code, message) ->
    `Internal_server_error, res id code message
;;

let parse_error () = Parse_error
let invalid_request ?data ?id () = Invalid_request (id, data)
let method_not_found ?id () = Method_not_found id
let invalid_params ?data ?id () = Invalid_params (id, data)
let internal_error ?id () = Internal_error id
let custom_error ?id ~code ~message () = Custom_error (id, code, message)

let no_supervised_directory ?id () =
  custom_error ?id ~code:32000 ~message:"No supervised directory" ()
;;

let validate_input =
  let open Rensai.Validation in
  record (fun f ->
    let open Record in
    let+ () = ensure f "jsonrpc" (string & String.equal "2.0")
    and+ meth = required f "method" string
    and+ id = optional f "id" int
    and+ params = optional f "params" ast in
    { id; meth; params })
;;

let from_response input =
  try
    input
    |> Yojson.Safe.from_string
    |> Rensai.Json.from_yojson
    |> validate_input
    |> Result.map_error (fun data -> invalid_request ~data ())
  with
  | _ -> Error (parse_error ())
;;

let result ?id to_rensai value =
  let open Rensai.Ast in
  Ok
    (record
       [ "jsonrpc", string "2.0"
       ; "id", option int id
       ; "result", to_rensai value
       ])
;;

let error x = x |> error_to_rensai |> snd

let handle input handler =
  Result.bind (input |> from_response) (fun { meth; params; id } ->
    handler meth params id)
  |> Result.fold ~ok:(fun res -> `OK, res) ~error:error_to_rensai
  |> fun (code, result) -> code, Rensai.Json.to_yojson result
;;

let handler meth ~params callback = meth, Handler (params, callback)

let services list input =
  handle input (fun meth params id ->
    match List.assoc_opt meth list with
    | None -> Error (method_not_found ?id ())
    | Some (Handler (validator, handler)) ->
      Result.bind
        (params
         |> validator
         |> Result.map_error (fun data -> invalid_params ~data ?id ()))
        (handler ?id))
;;
