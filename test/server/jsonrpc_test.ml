open Kohai_core
open Kohai_server
module Handler = Eff.Handler (struct end)

let dump = function
  | Ok value -> Fmt.str "%a" Rensai.Ast.pp value
  | Error error -> error |> Error.to_rensai |> Fmt.str "%a" Rensai.Ast.pp
;;

let nop = Rensai.Validation.const ()

let services =
  Jsonrpc.
    [ service
        ~meth:"test/ping"
        ~with_params:nop
        ~finalizer:Rensai.Ast.string
        (fun ?id:_ (module _ : Eff.HANDLER) () -> "pong")
    ; service
        ~meth:"test/echo"
        ~with_params:Rensai.Validation.string
        ~finalizer:Rensai.Ast.string
        (fun ?id:_ (module _ : Eff.HANDLER) value -> value)
    ; service
        ~meth:"test/rev"
        ~with_params:Rensai.Validation.string
        ~finalizer:Rensai.Ast.string
        (fun ?id:_ (module _ : Eff.HANDLER) value ->
           value
           |> String.to_seq
           |> List.of_seq
           |> List.rev
           |> List.to_seq
           |> String.of_seq)
    ]
;;

let run ?(services = []) input = Jsonrpc.run ~services input

let%expect_test "reacting to an input - 1" =
  let input = {json||json} in
  input |> run ~services |> Eff.handle (module Handler) |> dump |> print_endline;
  [%expect
    {|
    {error = {code = -32700; data = <null>; input = ""; message = "Parse error"};
      id = <null>; jsonrpc = "2.0"}
    |}]
;;

let%expect_test "reacting to an input - 2" =
  let input = {json|{}|json} in
  input |> run ~services |> Eff.handle (module Handler) |> dump |> print_endline;
  [%expect
    {|
    {error =
      {code = -32600;
        data =
         {unexpected_record = [{jsonrpc = "Missing"}; {method = "Missing"}];
           value = {}};
        input = "{}"; message = "Invalid Request"};
      id = <null>; jsonrpc = "2.0"}
    |}]
;;

let%expect_test "reacting to an input - 3" =
  let input = {json|{"jsonrpc": "2.0", "method": "foo", "id": 1}|json} in
  input |> run ~services |> Eff.handle (module Handler) |> dump |> print_endline;
  [%expect
    {|
    {error =
      {code = -32601; data = "foo";
        input = "{\"jsonrpc\": \"2.0\", \"method\": \"foo\", \"id\": 1}";
        message = "Method not found"};
      id = 1; jsonrpc = "2.0"}
    |}]
;;

let%expect_test "reacting to an input - 4" =
  let input = {json|{"jsonrpc": "1.0", "method": "foo", "id": 1}|json} in
  input |> run ~services |> Eff.handle (module Handler) |> dump |> print_endline;
  [%expect
    {|
    {error =
      {code = -32600;
        data =
         {unexpected_record =
           [{jsonrpc =
              {unexpected_value = "`a` (\"2.0\") is not equal to `b` (\"1.0\")"}}];
           value = {id = 1; jsonrpc = "1.0"; method = "foo"}};
        input = "{\"jsonrpc\": \"1.0\", \"method\": \"foo\", \"id\": 1}";
        message = "Invalid Request"};
      id = <null>; jsonrpc = "2.0"}
    |}]
;;

let%expect_test "reacting to an input - 5" =
  let input =
    {json|{"jsonrpc": "2.0", "method": "test/ping", "id": 333}|json}
  in
  input |> run ~services |> Eff.handle (module Handler) |> dump |> print_endline;
  [%expect {| {id = 333; jsonrpc = "2.0"; result = "pong"} |}]
;;

let%expect_test "reacting to an input - 6" =
  let input =
    {json|{"jsonrpc": "2.0", "method": "test/echo", "id": 333}|json}
  in
  input |> run ~services |> Eff.handle (module Handler) |> dump |> print_endline;
  [%expect
    {|
    {error =
      {code = -32602;
        data = {given = "null"; unexpected_kind = "string"; value = <null>};
        input = "{\"jsonrpc\": \"2.0\", \"method\": \"test/echo\", \"id\": 333}";
        message = "Invalid params"};
      id = 333; jsonrpc = "2.0"}
    |}]
;;

let%expect_test "reacting to an input - 7" =
  let input =
    {json|{"jsonrpc": "2.0", "method": "test/echo", "id": 333, "params": "foo"}|json}
  in
  input |> run ~services |> Eff.handle (module Handler) |> dump |> print_endline;
  [%expect {| {id = 333; jsonrpc = "2.0"; result = "foo"} |}]
;;

let%expect_test "reacting to an input - 8" =
  let input =
    {json|{"jsonrpc": "2.0", "method": "test/rev", "id": 333, "params": "bar"}|json}
  in
  input |> run ~services |> Eff.handle (module Handler) |> dump |> print_endline;
  [%expect {| {id = 333; jsonrpc = "2.0"; result = "rab"} |}]
;;
