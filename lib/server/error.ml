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

let custom_error ~body ?id ~code ?message () =
  Sigs.Custom_error { body; id; code; message }
;;
