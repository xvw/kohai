type t = Sigs.jsonrpc_error

let parse_error ~body () = Sigs.Parse_error { body }

let invalid_request ~body ?id ?error () =
  Sigs.Invalid_request { body; id; error }
;;

let method_not_found ~body ?id ~meth () =
  Sigs.Method_not_found { body; id; meth }
;;

let invalid_params ~body ?id ~error () = Sigs.Invalid_params { body; id; error }

let internal_error ~body ?id ?message () =
  Sigs.Internal_error { body; id; message }
;;

let custom_error ?(with_offset = true) ~body ?id ~code ?message () =
  let code = if with_offset then code + 32000 else code in
  Sigs.Custom_error { body; id; code; message }
;;

let unknown_error message () = custom_error ~code:99 ~body:"{}" ~message ()

let no_supervised_directory ~body ?id () =
  custom_error
    ~code:0
    ~message:"No supervised directory for the current session"
    ~body
    ?id
    ()
;;

let supervised_directory_error ~body ?id message () =
  custom_error ~code:1 ~message ~body ?id ()
;;

let no_related_transient_log ~body ?id index =
  custom_error
    ~code:2
    ~message:(Format.asprintf "transient log %d does not exists" index)
    ~body
    ?id
    ()
;;

let mk_error = Rensai.Validation.value_error_ast
let opt_error = Option.map mk_error
let mk_string = Rensai.Ast.string
let opt_string = Option.map mk_string

let to_rensai err =
  let res ?id ?data ~body ~code message =
    let open Rensai.Ast in
    record
      [ "jsonrpc", string "2.0"
      ; "id", option int id
      ; ( "error"
        , record
            [ "code", int (0 - abs code)
            ; "message", string message
            ; "data", option Fun.id data
            ; "input", string body
            ] )
      ]
  in
  match err with
  | Sigs.Parse_error { body } -> res ~body ~code:32700 "Parse error"
  | Sigs.Invalid_request { body; id; error } ->
    res ~body ?id ~code:32600 ?data:(opt_error error) "Invalid Request"
  | Sigs.Method_not_found { body; id; meth } ->
    res ~body ?id ~code:32601 ~data:(mk_string meth) "Method not found"
  | Sigs.Invalid_params { body; id; error } ->
    res ~body ?id ~code:32602 ~data:(mk_error error) "Invalid params"
  | Sigs.Internal_error { body; id; message } ->
    res ~body ?id ~code:32603 ?data:(opt_string message) "Internal error"
  | Sigs.Custom_error { body; id; code; message } ->
    res ~body ?id ~code ?data:(opt_string message) "Server error"
;;
