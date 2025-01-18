type t = string * Jsonrpc.handler

let nop x = Rensai.Validation.const () x
let return_string ?id = Jsonrpc.result ?id Rensai.Ast.string

let ping =
  Jsonrpc.handler "ping" ~params:nop (fun ?id () -> return_string ?id "pong")
;;

let ensure_supervised_directory supervised =
  Jsonrpc.handler "ensure_supervised_directory" ~params:nop (fun ?id () ->
    match !supervised with
    | Some x -> return_string ?id @@ Core.Path.to_string x
    | None -> Error (Jsonrpc.no_supervised_directory ?id ()))
;;
