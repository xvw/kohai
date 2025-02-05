open Kohai_server

let std_fs =
  Virtfs.(
    from_list [ dir "foo" []; dir "bar" [ dir "baz" [ dir "foobar" [] ] ] ])
;;

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
      let fs = std_fs
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
      let fs = std_fs
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
      let fs = std_fs
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
      let fs = std_fs
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
      let fs = std_fs
    end))
  in
  let input = input "kohai/supervision/set" ~params:{|"/foo"|} in
  input
  |> Jsonrpc.run ~services:Services.all
  |> Kohai_core.Eff.handle (module Handler)
  |> dump
  |> print_endline;
  [%expect {| {id = 1; jsonrpc = "2.0"; result = <null>} |}]
;;

let%expect_test "set supervised dir with valid path" =
  let module Handler =
    Kohai_core.Eff.Handler (Virtfs.Make (struct
      let fs = std_fs
    end))
  in
  let req0 =
    "kohai/supervision/get"
    |> input ~id:0
    |> Jsonrpc.run ~services:Services.all
    |> Kohai_core.Eff.handle (module Handler)
    |> dump
  in
  let req1 =
    "kohai/supervision/set"
    |> input ~id:1 ~params:{|"/foo"|}
    |> Jsonrpc.run ~services:Services.all
    |> Kohai_core.Eff.handle (module Handler)
    |> dump
  in
  let req2 =
    "kohai/supervision/get"
    |> input ~id:2
    |> Jsonrpc.run ~services:Services.all
    |> Kohai_core.Eff.handle (module Handler)
    |> dump
  in
  let req3 =
    "kohai/supervision/set"
    |> input ~id:3 ~params:{|"/oo"|}
    |> Jsonrpc.run ~services:Services.all
    |> Kohai_core.Eff.handle (module Handler)
    |> dump
  in
  let req4 =
    "kohai/supervision/get"
    |> input ~id:4
    |> Jsonrpc.run ~services:Services.all
    |> Kohai_core.Eff.handle (module Handler)
    |> dump
  in
  List.iter print_endline [ req0; req1; req2; req3; req4 ];
  [%expect
    {|
    {id = 0; jsonrpc = "2.0"; result = <null>}
    {id = 1; jsonrpc = "2.0"; result = <null>}
    {id = 2; jsonrpc = "2.0"; result = "/foo"}
    {error =
      {code = -32001; data = "The given directory does not exists";
        input =
         "{\"jsonrpc\": \"2.0\", \"method\": \"kohai/supervision/set\", \"id\": 3, \"params\": \"/oo\"}";
        message = "Server error"};
      id = 3; jsonrpc = "2.0"}
    {id = 4; jsonrpc = "2.0"; result = "/foo"}
    |}]
;;

let%expect_test "set supervised dir with valid path" =
  let module Handler =
    Kohai_core.Eff.Handler (Virtfs.Make (struct
      let fs = std_fs
    end))
  in
  let req0 =
    "kohai/supervision/is_valid"
    |> input ~id:0 ~params:{|"./foo"|}
    |> Jsonrpc.run ~services:Services.all
    |> Kohai_core.Eff.handle (module Handler)
    |> dump
  in
  let req1 =
    "kohai/supervision/is_valid"
    |> input ~id:1 ~params:{|"/foo"|}
    |> Jsonrpc.run ~services:Services.all
    |> Kohai_core.Eff.handle (module Handler)
    |> dump
  in
  let req2 =
    "kohai/supervision/is_valid"
    |> input ~id:2 ~params:{|"/bar"|}
    |> Jsonrpc.run ~services:Services.all
    |> Kohai_core.Eff.handle (module Handler)
    |> dump
  in
  let req3 =
    "kohai/supervision/is_valid"
    |> input ~id:3 ~params:{|"/bar/baz"|}
    |> Jsonrpc.run ~services:Services.all
    |> Kohai_core.Eff.handle (module Handler)
    |> dump
  in
  let req4 =
    "kohai/supervision/is_valid"
    |> input ~id:4 ~params:{|"/bar/baz/foobar"|}
    |> Jsonrpc.run ~services:Services.all
    |> Kohai_core.Eff.handle (module Handler)
    |> dump
  in
  let req5 =
    "kohai/supervision/is_valid"
    |> input ~id:5 ~params:{|"/bar/foobar"|}
    |> Jsonrpc.run ~services:Services.all
    |> Kohai_core.Eff.handle (module Handler)
    |> dump
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

let%expect_test "get sectors without files" =
  let module Handler =
    Kohai_core.Eff.Handler (Virtfs.Make (struct
      let fs = Virtfs.(from_list [ dir "supervised" [] ])
    end))
  in
  let req0 =
    "kohai/supervision/set"
    |> input ~id:0 ~params:{|"/supervised"|}
    |> Jsonrpc.run ~services:Services.all
    |> Kohai_core.Eff.handle (module Handler)
    |> dump
  in
  let req1 =
    "kohai/sector/list"
    |> input ~id:1
    |> Jsonrpc.run ~services:Services.all
    |> Kohai_core.Eff.handle (module Handler)
    |> dump
  in
  List.iter print_endline [ req0; req1 ];
  [%expect
    {|
    {id = 0; jsonrpc = "2.0"; result = <null>}
    {id = 1; jsonrpc = "2.0"; result = []}
    |}]
;;

let%expect_test "get sectors with files" =
  let content =
    [ {|<name: "programming"; description: "sector about programming">|}
    ; {|<name: "art">|}
    ; {|<name: "cooking"; description: "sector about cooking">|}
    ]
    |> String.concat "\n"
  in
  let module Handler =
    Kohai_core.Eff.Handler (Virtfs.Make (struct
      let fs =
        Virtfs.(
          from_list
            [ dir "supervised" [ dir "list" [ file ~content "sectors.rens" ] ] ])
      ;;
    end))
  in
  let req0 =
    "kohai/supervision/set"
    |> input ~id:0 ~params:{|"/supervised"|}
    |> Jsonrpc.run ~services:Services.all
    |> Kohai_core.Eff.handle (module Handler)
    |> dump
  in
  let req1 =
    "kohai/sector/list"
    |> input ~id:1
    |> Jsonrpc.run ~services:Services.all
    |> Kohai_core.Eff.handle (module Handler)
    |> dump
  in
  List.iter print_endline [ req0; req1 ];
  [%expect
    {|
    {id = 0; jsonrpc = "2.0"; result = <null>}
    {id = 1; jsonrpc = "2.0";
      result =
       [{description = "sector about cooking"; name = "cooking"};
        {description = <null>; name = "art"};
        {description = "sector about programming"; name = "programming"}]}
    |}]
;;

let%expect_test "store sectors" =
  let content =
    [ {|<name: "programming"; description: "sector about programming">|}
    ; {|<name: "art">|}
    ; {|<name: "cooking"; description: "sector about cooking">|}
    ]
    |> String.concat "\n"
  in
  let module V =
    Virtfs.Make (struct
      let fs =
        Virtfs.(
          from_list
            [ dir "supervised" [ dir "list" [ file ~content "sectors.rens" ] ] ])
      ;;
    end)
  in
  let module Handler = Kohai_core.Eff.Handler (V) in
  let req0 =
    "kohai/supervision/set"
    |> input ~id:0 ~params:{|"/supervised"|}
    |> Jsonrpc.run ~services:Services.all
    |> Kohai_core.Eff.handle (module Handler)
    |> dump
  in
  let req1 =
    "kohai/sector/list"
    |> input ~id:1
    |> Jsonrpc.run ~services:Services.all
    |> Kohai_core.Eff.handle (module Handler)
    |> dump
  in
  let req2 =
    "kohai/sector/save"
    |> input ~id:2 ~params:{|{"name": "learn"}|}
    |> Jsonrpc.run ~services:Services.all
    |> Kohai_core.Eff.handle (module Handler)
    |> dump
  in
  let req3 =
    "kohai/sector/save"
    |> input ~id:3 ~params:{|{"name": "art", "description": "about art"}|}
    |> Jsonrpc.run ~services:Services.all
    |> Kohai_core.Eff.handle (module Handler)
    |> dump
  in
  let req4 =
    "kohai/sector/list"
    |> input ~id:4
    |> Jsonrpc.run ~services:Services.all
    |> Kohai_core.Eff.handle (module Handler)
    |> dump
  in
  List.iter print_endline [ req0; req1; req2; req3; req4 ];
  [%expect
    {|
    {id = 0; jsonrpc = "2.0"; result = <null>}
    {id = 1; jsonrpc = "2.0";
      result =
       [{description = "sector about cooking"; name = "cooking"};
        {description = <null>; name = "art"};
        {description = "sector about programming"; name = "programming"}]}
    {id = 2; jsonrpc = "2.0";
      result =
       [{description = "sector about programming"; name = "programming"};
        {description = <null>; name = "learn"};
        {description = "sector about cooking"; name = "cooking"};
        {description = <null>; name = "art"}]}
    {id = 3; jsonrpc = "2.0";
      result =
       [{description = "sector about programming"; name = "programming"};
        {description = <null>; name = "learn"};
        {description = "sector about cooking"; name = "cooking"};
        {description = "about art"; name = "art"}]}
    {id = 4; jsonrpc = "2.0";
      result =
       [{description = "about art"; name = "art"};
        {description = "sector about cooking"; name = "cooking"};
        {description = <null>; name = "learn"};
        {description = "sector about programming"; name = "programming"}]}
    |}]
;;
