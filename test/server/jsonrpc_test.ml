open Kohai_core
open Kohai_server

module Handler = Eff.Handler (struct
    let exists _ = true
    let is_file _ = true
    let is_dir _ = true
    let read_file _ = ""
    let create_dir _ = ()
    let write_file _ _ = ()
    let append_to_file _ _ = ()
    let delete_file _ = ()
    let delete_dir ?recursive:_ _ = ()
    let now () = 0.0
    let datetime_from_float _ = Ok Datetime.unix
    let set_supervised_directory _ = ()
    let get_supervised_directory () = None
  end)

let dump = function
  | Ok value -> Fmt.str "%a" Rensai.Ast.pp value
  | Error error -> error |> Error.to_rensai |> Fmt.str "%a" Rensai.Ast.pp
;;

let nop = Rensai.Validation.const ()

let services _body =
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

let run ?(services = fun _ -> []) input = Jsonrpc.run ~services input

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

let%expect_test "List of methods - without prefix" =
  let input =
    {json|{"jsonrpc": "2.0", "method": "admin/methods", "id": 1}|json}
  in
  input
  |> run ~services:Services.all
  |> Eff.handle (module Handler)
  |> dump
  |> print_endline;
  [%expect
    {|
    {id = 1; jsonrpc = "2.0";
      result =
       ["admin/methods"; "experimental/ping"; "experimental/echo";
        "experimental/plus"; "kohai/supervision/ensure";
        "kohai/supervision/is_valid"; "kohai/supervision/get";
        "kohai/supervision/set"; "kohai/sector/list"; "kohai/sector/save";
        "kohai/sector/get"; "kohai/sector/delete"; "kohai/project/list";
        "kohai/project/save"; "kohai/project/get"; "kohai/project/delete";
        "kohai/transient-log/list"; "kohai/transient-log/get";
        "kohai/transient-log/action"; "kohai/state/get";
        "kohai/state/get/sector"; "kohai/state/get/project"; "kohai/log/get";
        "kohai/log/last"; "kohai/log/last/sector"; "kohai/log/last/project";
        "kohai/log/unpromote"]}
    |}]
;;

let%expect_test "List of methods - with prefix 1" =
  let input =
    {json|{"jsonrpc": "2.0", "params": "admin/",  "method": "admin/methods", "id": 1}|json}
  in
  input
  |> run ~services:Services.all
  |> Eff.handle (module Handler)
  |> dump
  |> print_endline;
  [%expect {| {id = 1; jsonrpc = "2.0"; result = ["admin/methods"]} |}]
;;

let%expect_test "List of methods - with prefix 2" =
  let input =
    {json|{"jsonrpc": "2.0", "params": "exp",  "method": "admin/methods", "id": 1}|json}
  in
  input
  |> run ~services:Services.all
  |> Eff.handle (module Handler)
  |> dump
  |> print_endline;
  [%expect
    {|
    {id = 1; jsonrpc = "2.0";
      result = ["experimental/ping"; "experimental/echo"; "experimental/plus"]}
    |}]
;;

let%expect_test "echo - 1" =
  let input =
    {json|{"jsonrpc": "2.0",
           "params": "foobar",
           "method": "experimental/echo",
           "id": 1}|json}
  in
  input
  |> run ~services:Services.all
  |> Eff.handle (module Handler)
  |> dump
  |> print_endline;
  [%expect {| {id = 1; jsonrpc = "2.0"; result = "foobar"} |}]
;;

let%expect_test "ping - 1" =
  let input =
    {json|{"jsonrpc": "2.0",
           "method": "experimental/ping",
           "id": 1}|json}
  in
  input
  |> run ~services:Services.all
  |> Eff.handle (module Handler)
  |> dump
  |> print_endline;
  [%expect {| {id = 1; jsonrpc = "2.0"; result = "pong"} |}]
;;

let%expect_test "plus - 1" =
  let input =
    {json|{"jsonrpc": "2.0",
           "params": [42, 39],
           "method": "experimental/plus",
           "id": 1}|json}
  in
  input
  |> run ~services:Services.all
  |> Eff.handle (module Handler)
  |> dump
  |> print_endline;
  [%expect {| {id = 1; jsonrpc = "2.0"; result = 81} |}]
;;

let%expect_test "plus - 2" =
  let input =
    {json|{"jsonrpc": "2.0",
           "params": ["42L", "39.33"],
           "method": "experimental/plus",
           "id": 1}|json}
  in
  input
  |> run ~services:Services.all
  |> Eff.handle (module Handler)
  |> dump
  |> print_endline;
  [%expect {| {id = 1; jsonrpc = "2.0"; result = 81.33} |}]
;;

let%expect_test "plus - 3" =
  let input =
    {json|{"jsonrpc": "2.0",
           "params": [39.33],
           "method": "experimental/plus",
           "id": 1}|json}
  in
  input
  |> run ~services:Services.all
  |> Eff.handle (module Handler)
  |> dump
  |> print_endline;
  [%expect
    {|
    {error =
      {code = -32602;
        data =
         {given = "list<float>"; unexpected_kind = "(?any, ?any)";
           value = [39.33]};
        input =
         "{\"jsonrpc\": \"2.0\",\n           \"params\": [39.33],\n           \"method\": \"experimental/plus\",\n           \"id\": 1}";
        message = "Invalid params"};
      id = 1; jsonrpc = "2.0"}
    |}]
;;
