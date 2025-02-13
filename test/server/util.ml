let request_input ?(id = 1) ?params meth =
  Format.asprintf
    {j|{"jsonrpc": "2.0", "method": "%s", "id": %d%s}|j}
    meth
    id
    (match params with
     | None -> ""
     | Some x -> Format.asprintf {|, "params": %s|} x)
;;

let request_dump = function
  | Ok value -> Fmt.str "%a" Rensai.Ast.pp value
  | Error error ->
    error |> Kohai_server.Error.to_rensai |> Fmt.str "%a" Rensai.Ast.pp
;;
