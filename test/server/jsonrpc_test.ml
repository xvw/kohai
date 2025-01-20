open Kohai_server

let dump result =
  Format.asprintf
    "%a"
    Rensai.Ast.pp
    (match result () with
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
  ; Jsonrpc.handler
      ~meth:"echo"
      ~with_params:Rensai.Validation.string
      ~finalizer:Rensai.Ast.string
      (fun ?id:_ result -> Core.IO.return result)
  ]
;;

let%expect_test "reacting to an input - 1" =
  let input = {json| {"jsonrpc": "2.0", "method": "ping", "id": 1} |json} in
  let endpoints = Jsonrpc.services handlers in
  input |> endpoints |> dump;
  [%expect {| {id = 1; jsonrpc = "2.0"; result = "pong"} |}]
;;

let%expect_test "reacting to an input - 2" =
  let input = {json| {"jsonrpc": "2.0", "method": "pung", "id": 1} |json} in
  let endpoints = Jsonrpc.services handlers in
  input |> endpoints |> dump;
  [%expect
    {|
    {error = {code = -32601; data = "pung"; message = "Method not found"};
      id = 1; jsonrpc = "2.0"}
    |}]
;;

let%expect_test "reacting to an input - 3" =
  let input =
    {json| {"jsonrpc": "2.0", "method": "echo", "params": "foo", "id": 1} |json}
  in
  let endpoints = Jsonrpc.services handlers in
  input |> endpoints |> dump;
  [%expect {| {id = 1; jsonrpc = "2.0"; result = "foo"} |}]
;;

let%expect_test "reacting to an input - 4" =
  let input =
    {json| {"jsonrpc": "2.0", "method": "echo", "params": {}, "id": 1} |json}
  in
  let endpoints = Jsonrpc.services handlers in
  input |> endpoints |> dump;
  [%expect
    {|
    {error =
      {code = -32602;
        data = {given = "?record"; unexpected_kind = "string"; value = {}};
        message = "Invalid params"};
      id = 1; jsonrpc = "2.0"}
    |}]
;;

let%expect_test "reacting to an input - 5" =
  let input =
    {json| {"jsonrpc": "2.0", "method": "echo", "params: {}, "id": 1} |json}
  in
  let endpoints = Jsonrpc.services handlers in
  input |> endpoints |> dump;
  [%expect
    {|
    {error = {code = -32700; data = <null>; message = "Parse error"};
      id = <null>; jsonrpc = "2.0"}
    |}]
;;

let%expect_test "reacting to an input - 5" =
  let input =
    {json| {"jsonrpc": "1.0", "method": "echo", "params": {}, "id": 1} |json}
  in
  let endpoints = Jsonrpc.services handlers in
  input |> endpoints |> dump;
  [%expect
    {|
    {error =
      {code = -32600;
        data =
         {unexpected_record =
           [{jsonrpc =
              {unexpected_value = "`a` (\"2.0\") is not equal to `b` (\"1.0\")"}}];
           value = {id = 1; jsonrpc = "1.0"; method = "echo"; params = {}}};
        message = "Invalid Request"};
      id = <null>; jsonrpc = "2.0"}
    |}]
;;
