open Kohai_server
open Util

let std_fs =
  Virtfs.(
    from_list [ dir "foo" []; dir "bar" [ dir "baz" [ dir "foobar" [] ] ] ])
;;

let%expect_test "ensure supervision - not supervised" =
  let module Handler =
    Kohai_core.Eff.Handler (Virtfs.Make (struct
      let fs = std_fs
      let now = Kohai_core.Datetime.unix
    end))
  in
  let input = request_input ~id:42 "kohai/supervision/ensure" in
  input
  |> Jsonrpc.run (module Handler) ~services:Services.all
  |> request_dump
  |> print_endline;
  [%expect
    {|
    {error =
      {body =
        "{\"jsonrpc\": \"2.0\", \"method\": \"kohai/supervision/ensure\", \"id\": 42}";
        code = -32000; data = "No supervised directory for the current session";
        message = "Server error"};
      id = 42; jsonrpc = "2.0"}
    |}]
;;

let%expect_test "get supervised directory" =
  let module Handler =
    Kohai_core.Eff.Handler (Virtfs.Make (struct
      let fs = std_fs
      let now = Kohai_core.Datetime.unix
    end))
  in
  let input = request_input "kohai/supervision/get" in
  input
  |> Jsonrpc.run (module Handler) ~services:Services.all
  |> request_dump
  |> print_endline;
  [%expect {| {id = 1; jsonrpc = "2.0"; result = <null>} |}]
;;

let%expect_test "set supervised dir with relative path" =
  let module Handler =
    Kohai_core.Eff.Handler (Virtfs.Make (struct
      let fs = std_fs
      let now = Kohai_core.Datetime.unix
    end))
  in
  let input = request_input "kohai/supervision/set" ~params:{|"./foo"|} in
  input
  |> Jsonrpc.run (module Handler) ~services:Services.all
  |> request_dump
  |> print_endline;
  [%expect
    {|
    {error =
      {body =
        "{\"jsonrpc\": \"2.0\", \"method\": \"kohai/supervision/set\", \"id\": 1, \"params\": \"./foo\"}";
        code = -32001; data = "Supervised directory need to be absolute";
        message = "Server error"};
      id = 1; jsonrpc = "2.0"}
    |}]
;;

let%expect_test "set supervised dir with inexistant path" =
  let module Handler =
    Kohai_core.Eff.Handler (Virtfs.Make (struct
      let fs = std_fs
      let now = Kohai_core.Datetime.unix
    end))
  in
  let input =
    request_input "kohai/supervision/set" ~params:{|"/skdsajdsakjdjk"|}
  in
  input
  |> Jsonrpc.run (module Handler) ~services:Services.all
  |> request_dump
  |> print_endline;
  [%expect
    {|
    {error =
      {body =
        "{\"jsonrpc\": \"2.0\", \"method\": \"kohai/supervision/set\", \"id\": 1, \"params\": \"/skdsajdsakjdjk\"}";
        code = -32001; data = "The given directory does not exists";
        message = "Server error"};
      id = 1; jsonrpc = "2.0"}
    |}]
;;

let%expect_test "set supervised dir with valid path" =
  let module Handler =
    Kohai_core.Eff.Handler (Virtfs.Make (struct
      let fs = std_fs
      let now = Kohai_core.Datetime.unix
    end))
  in
  let input = request_input "kohai/supervision/set" ~params:{|"/foo"|} in
  input
  |> Jsonrpc.run (module Handler) ~services:Services.all
  |> request_dump
  |> print_endline;
  [%expect {| {id = 1; jsonrpc = "2.0"; result = "/foo"} |}]
;;

let%expect_test "set supervised dir with valid path" =
  let module Handler =
    Kohai_core.Eff.Handler (Virtfs.Make (struct
      let fs = std_fs
      let now = Kohai_core.Datetime.unix
    end))
  in
  let req0 =
    "kohai/supervision/get"
    |> request_input ~id:0
    |> Jsonrpc.run (module Handler) ~services:Services.all
    |> request_dump
  in
  let req1 =
    "kohai/supervision/set"
    |> request_input ~id:1 ~params:{|"/foo"|}
    |> Jsonrpc.run (module Handler) ~services:Services.all
    |> request_dump
  in
  let req2 =
    "kohai/supervision/get"
    |> request_input ~id:2
    |> Jsonrpc.run (module Handler) ~services:Services.all
    |> request_dump
  in
  let req3 =
    "kohai/supervision/set"
    |> request_input ~id:3 ~params:{|"/oo"|}
    |> Jsonrpc.run (module Handler) ~services:Services.all
    |> request_dump
  in
  let req4 =
    "kohai/supervision/get"
    |> request_input ~id:4
    |> Jsonrpc.run (module Handler) ~services:Services.all
    |> request_dump
  in
  List.iter print_endline [ req0; req1; req2; req3; req4 ];
  [%expect
    {|
    {id = 0; jsonrpc = "2.0"; result = <null>}
    {id = 1; jsonrpc = "2.0"; result = "/foo"}
    {id = 2; jsonrpc = "2.0"; result = "/foo"}
    {error =
      {body =
        "{\"jsonrpc\": \"2.0\", \"method\": \"kohai/supervision/set\", \"id\": 3, \"params\": \"/oo\"}";
        code = -32001; data = "The given directory does not exists";
        message = "Server error"};
      id = 3; jsonrpc = "2.0"}
    {id = 4; jsonrpc = "2.0"; result = "/foo"}
    |}]
;;

let%expect_test "set supervised dir with valid path" =
  let module Handler =
    Kohai_core.Eff.Handler (Virtfs.Make (struct
      let fs = std_fs
      let now = Kohai_core.Datetime.unix
    end))
  in
  let req0 =
    "kohai/supervision/is_valid"
    |> request_input ~id:0 ~params:{|"./foo"|}
    |> Jsonrpc.run (module Handler) ~services:Services.all
    |> request_dump
  in
  let req1 =
    "kohai/supervision/is_valid"
    |> request_input ~id:1 ~params:{|"/foo"|}
    |> Jsonrpc.run (module Handler) ~services:Services.all
    |> request_dump
  in
  let req2 =
    "kohai/supervision/is_valid"
    |> request_input ~id:2 ~params:{|"/bar"|}
    |> Jsonrpc.run (module Handler) ~services:Services.all
    |> request_dump
  in
  let req3 =
    "kohai/supervision/is_valid"
    |> request_input ~id:3 ~params:{|"/bar/baz"|}
    |> Jsonrpc.run (module Handler) ~services:Services.all
    |> request_dump
  in
  let req4 =
    "kohai/supervision/is_valid"
    |> request_input ~id:4 ~params:{|"/bar/baz/foobar"|}
    |> Jsonrpc.run (module Handler) ~services:Services.all
    |> request_dump
  in
  let req5 =
    "kohai/supervision/is_valid"
    |> request_input ~id:5 ~params:{|"/bar/foobar"|}
    |> Jsonrpc.run (module Handler) ~services:Services.all
    |> request_dump
  in
  List.iter print_endline [ req0; req1; req2; req3; req4; req5 ];
  [%expect
    {|
    {id = 0; jsonrpc = "2.0"; result = false}
    {id = 1; jsonrpc = "2.0"; result = true}
    {id = 2; jsonrpc = "2.0"; result = true}
    {id = 3; jsonrpc = "2.0"; result = true}
    {id = 4; jsonrpc = "2.0"; result = true}
    {id = 5; jsonrpc = "2.0"; result = false}
    |}]
;;
