open Kohai_server

let dump result =
  Format.asprintf
    "%a"
    Rensai.Ast.pp
    (match Core.IO.force result with
     | Error xs -> Error.to_rensai xs
     | Ok x -> x)
  |> print_endline
;;

let nop = Rensai.Validation.const ()

let handlers =
  [ Jsonrpc.handler
      ~meth:"ping"
      ~with_params:nop
      ~finalizer:Rensai.Ast.string
      (fun ?id:_ () -> Core.IO.return "pong")
  ]
;;

let%expect_test "reacting to an input - 1" =
  let input = {json| {"jsonrpc": "2.0", "method": "ping", "id": 1} |json} in
  let endpoints = Jsonrpc.services handlers Fun.id in
  input |> endpoints |> dump;
  [%expect {| {id = 1; jsonrpc = "2.0"; result = "pong"} |}]
;;

let%expect_test "reacting to an input - 2" =
  let input = {json| {"jsonrpc": "2.0", "method": "pung", "id": 1} |json} in
  let endpoints = Jsonrpc.services handlers Fun.id in
  input |> endpoints |> dump;
  [%expect {| {id = 1; jsonrpc = "2.0"; result = "pong"} |}]
;;
