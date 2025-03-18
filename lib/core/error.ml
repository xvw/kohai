type t =
  | Parse_error of { body : string }
  | Invalid_request of
      { body : string
      ; error : Rensai.Validation.value_error
      }
  | Method_not_found of
      { body : string
      ; id : int option
      ; meth : string
      }
  | Invalid_params of
      { body : string
      ; id : int option
      ; error : Rensai.Validation.value_error
      }
  | Internal_error of
      { body : string
      ; id : int option
      ; message : string
      }
  | Custom_error of
      { body : string
      ; id : int option
      ; code : int
      ; message : string option
      }

type custom =
  | Unknown_error of string
  | No_supervised_directory
  | Supervised_directory_error of string
  | Invalid_datetime of
      { value : float
      ; error : Rensai.Validation.value_error
      }
  | Resource_not_found of
      { index : string
      ; subject : string
      ; code : int
      }

let parse_error ~body () = Parse_error { body }
let invalid_request ~body ~error () = Invalid_request { body; error }
let method_not_found ~body ?id ~meth () = Method_not_found { body; id; meth }
let invalid_params ~body ?id ~error () = Invalid_params { body; id; error }
let internal_error ~body ?id ~message () = Internal_error { body; id; message }

let custom_error ?(with_offset = true) ~body ?id ?(code = 0) ?message () =
  Custom_error
    { body; id; code = (if with_offset then code + 32000 else code); message }
;;

let invalid_datetime value error = Invalid_datetime { value; error }
let unknown_error ~message () = Unknown_error message
let no_supervised_directory () = No_supervised_directory
let supervised_directory_error ~message () = Supervised_directory_error message

let resource_not_found ~index ~subject ~code () =
  Resource_not_found { index; subject; code }
;;

let no_related_transient_log ~index () =
  resource_not_found
    ~index:(string_of_int index)
    ~subject:"transient log"
    ~code:0
    ()
;;

let no_related_log ~uuid () =
  resource_not_found ~index:(Uuid.to_string uuid) ~subject:"log" ~code:1 ()
;;

let custom_to_jsonrpc ~body ?id = function
  | Unknown_error message -> custom_error ~body ?id ~code:99 ~message ()
  | No_supervised_directory ->
    custom_error
      ~body
      ?id
      ~code:0
      ~message:"No supervised directory for the current session"
      ()
  | Supervised_directory_error message ->
    custom_error ~body ?id ~code:1 ~message ()
  | Invalid_datetime { value; error } ->
    custom_error
      ~body
      ?id
      ~code:2
      ~message:
        (Format.asprintf
           "%f is not a valid datetime for %a"
           value
           Rensai.Validation.pp_value_error
           error)
      ()
  | Resource_not_found { index; subject; code } ->
    custom_error
      ~body
      ?id
      ~code:(10 + code)
      ~message:(Format.asprintf "[%s#%s] not found" subject index)
      ()
;;

let mk_error = Rensai.Validation.value_error_ast
let mk_string = Rensai.Ast.string
let opt_string = Option.map mk_string

let result ?id ?data ~body ~code message =
  let open Rensai.Ast in
  record
    [ "jsonrpc", string "2.0"
    ; "id", option int id
    ; ( "error"
      , record
          [ "code", int (0 - abs code)
          ; "message", string message
          ; "data", option Fun.id data
          ; "body", string body
          ] )
    ]
;;

let jsonrpc_to_rensai = function
  | Parse_error { body } -> result ~body ~code:32700 "Parse error"
  | Invalid_request { body; error } ->
    result ~body ~code:32600 ~data:(mk_error error) "Invalid request"
  | Method_not_found { body; id; meth } ->
    result ~body ~code:32601 ?id ~data:(mk_string meth) "Method not found"
  | Invalid_params { body; id; error } ->
    result ~body ?id ~code:32602 ~data:(mk_error error) "Invalid params"
  | Internal_error { body; id; message } ->
    result ~body ?id ~code:32603 ~data:(mk_string message) "Internal error"
  | Custom_error { body; id; code; message } ->
    result ~body ?id ~code ?data:(opt_string message) "Server error"
;;

let to_rensai = jsonrpc_to_rensai
