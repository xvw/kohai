open Kohai_server

let dump = function
  | Ok value -> Fmt.str "%a" Rensai.Ast.pp value
  | Error error -> error |> Error.to_rensai |> Fmt.str "%a" Rensai.Ast.pp
;;

let input ?(id = 1) ?params meth =
  Format.asprintf
    {j|{"jsonrpc": "2.0", "method": "%s", "id": %d%s}|j}
    meth
    id
    (match params with
     | None -> ""
     | Some x -> Format.asprintf {|, "params": %s|} x)
;;

let%expect_test "ensure supervision - not supervised" =
  let module Handler =
    Kohai_core.Eff.Handler (Virtfs.Make (struct
      let fs = Virtfs.(from_list [ dir "foo" []; dir "bar" [] ])
    end))
  in
  let input = input ~id:42 "kohai/supervision/ensure" in
  input
  |> Jsonrpc.run ~services:Services.all
  |> Kohai_core.Eff.handle (module Handler)
  |> dump
  |> print_endline;
  [%expect
    {|
    {error =
      {code = -32000; data = "No supervised directory for the current session";
        input =
         "{\"jsonrpc\": \"2.0\", \"method\": \"kohai/supervision/ensure\", \"id\": 42}";
        message = "Server error"};
      id = 42; jsonrpc = "2.0"}
    |}]
;;

let%expect_test "get supervised directory" =
  let module Handler =
    Kohai_core.Eff.Handler (Virtfs.Make (struct
      let fs = Virtfs.(from_list [ dir "foo" []; dir "bar" [] ])
    end))
  in
  let input = input "kohai/supervision/get" in
  input
  |> Jsonrpc.run ~services:Services.all
  |> Kohai_core.Eff.handle (module Handler)
  |> dump
  |> print_endline;
  [%expect {| {id = 1; jsonrpc = "2.0"; result = <null>} |}]
;;

let%expect_test "set supervised dir with relative path" =
  let module Handler =
    Kohai_core.Eff.Handler (Virtfs.Make (struct
      let fs = Virtfs.(from_list [ dir "foo" []; dir "bar" [] ])
    end))
  in
  let input = input "kohai/supervision/set" ~params:{|"./foo"|} in
  input
  |> Jsonrpc.run ~services:Services.all
  |> Kohai_core.Eff.handle (module Handler)
  |> dump
  |> print_endline;
  [%expect
    {|
    {error =
      {code = -32001; data = "Supervised directory need to be absolute";
        input =
         "{\"jsonrpc\": \"2.0\", \"method\": \"kohai/supervision/set\", \"id\": 1, \"params\": \"./foo\"}";
        message = "Server error"};
      id = 1; jsonrpc = "2.0"}
    |}]
;;

let%expect_test "set supervised dir with inexistant path" =
  let module Handler =
    Kohai_core.Eff.Handler (Virtfs.Make (struct
      let fs = Virtfs.(from_list [ dir "foo" []; dir "bar" [] ])
    end))
  in
  let input = input "kohai/supervision/set" ~params:{|"/skdsajdsakjdjk"|} in
  input
  |> Jsonrpc.run ~services:Services.all
  |> Kohai_core.Eff.handle (module Handler)
  |> dump
  |> print_endline;
  [%expect
    {|
    {error =
      {code = -32001; data = "The given directory does not exists";
        input =
         "{\"jsonrpc\": \"2.0\", \"method\": \"kohai/supervision/set\", \"id\": 1, \"params\": \"/skdsajdsakjdjk\"}";
        message = "Server error"};
      id = 1; jsonrpc = "2.0"}
    |}]
;;

let%expect_test "set supervised dir with valid path" =
  let module Handler =
    Kohai_core.Eff.Handler (Virtfs.Make (struct
      let fs = Virtfs.(from_list [ dir "foo" []; dir "bar" [] ])
    end))
  in
  let input = input "kohai/supervision/set" ~params:{|"/foo"|} in
  input
  |> Jsonrpc.run ~services:Services.all
  |> Kohai_core.Eff.handle (module Handler)
  |> dump
  |> print_endline;
  [%expect
    {| {id = 1; jsonrpc = "2.0"; result = <null>} |}]
;;
