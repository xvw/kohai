type t = string * Jsonrpc.handler

open Core
module V = Rensai.Validation
module A = Rensai.Ast

let nop x = V.const () x
let return_string ?id = Jsonrpc.result ?id A.string

let ping =
  Jsonrpc.handler "ping" ~params:nop (fun ?id () -> return_string ?id "pong")
;;

let ensure_supervised_directory supervised =
  Jsonrpc.handler "directory/ensure" ~params:nop (fun ?id () ->
    match !supervised with
    | Some x -> return_string ?id @@ Path.to_string x
    | None -> Error (Jsonrpc.no_supervised_directory ?id ()))
;;

let set_supervised_directory supervised =
  Jsonrpc.handler
    "directory/set"
    ~params:(V.option Path.from_rensai)
    (fun ?id -> function
    | None -> Error (Jsonrpc.no_supervised_directory ?id ())
    | Some path when Path.is_relative path ->
      Error (Jsonrpc.no_supervised_directory ?id ())
    | Some path ->
      let () = supervised := Some path in
      return_string ?id (Path.to_string path))
;;
