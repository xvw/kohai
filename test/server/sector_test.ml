open Kohai_server
open Util

let%expect_test "get sectors without files" =
  let module Handler =
    Kohai_core.Eff.Handler (Virtfs.Make (struct
      let fs = Virtfs.(from_list [ dir "supervised" [] ])
      let now = Kohai_core.Datetime.unix
    end))
  in
  let req0 =
    "kohai/supervision/set"
    |> request_input ~id:0 ~params:{|"/supervised"|}
    |> Jsonrpc.run ~services:Services.all
    |> Kohai_core.Eff.handle (module Handler)
    |> request_dump
  in
  let req1 =
    "kohai/sector/list"
    |> request_input ~id:1
    |> Jsonrpc.run ~services:Services.all
    |> Kohai_core.Eff.handle (module Handler)
    |> request_dump
  in
  List.iter print_endline [ req0; req1 ];
  [%expect
    {|
    {id = 0; jsonrpc = "2.0"; result = "/supervised"}
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

      let now = Kohai_core.Datetime.unix
    end))
  in
  let req0 =
    "kohai/supervision/set"
    |> request_input ~id:0 ~params:{|"/supervised"|}
    |> Jsonrpc.run ~services:Services.all
    |> Kohai_core.Eff.handle (module Handler)
    |> request_dump
  in
  let req1 =
    "kohai/sector/list"
    |> request_input ~id:1
    |> Jsonrpc.run ~services:Services.all
    |> Kohai_core.Eff.handle (module Handler)
    |> request_dump
  in
  List.iter print_endline [ req0; req1 ];
  [%expect
    {|
    {id = 0; jsonrpc = "2.0"; result = "/supervised"}
    {id = 1; jsonrpc = "2.0";
      result =
       [{counter = 0; description = <null>; name = "art"};
        {counter = 0; description = "sector about cooking"; name = "cooking"};
        {counter = 0; description = "sector about programming";
          name = "programming"}]}
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

      let now = Kohai_core.Datetime.unix
    end)
  in
  let module Handler = Kohai_core.Eff.Handler (V) in
  let req0 =
    "kohai/supervision/set"
    |> request_input ~id:0 ~params:{|"/supervised"|}
    |> Jsonrpc.run ~services:Services.all
    |> Kohai_core.Eff.handle (module Handler)
    |> request_dump
  in
  let req1 =
    "kohai/sector/list"
    |> request_input ~id:1
    |> Jsonrpc.run ~services:Services.all
    |> Kohai_core.Eff.handle (module Handler)
    |> request_dump
  in
  let req2 =
    "kohai/sector/save"
    |> request_input ~id:2 ~params:{|{"name": "learn"}|}
    |> Jsonrpc.run ~services:Services.all
    |> Kohai_core.Eff.handle (module Handler)
    |> request_dump
  in
  let req3 =
    "kohai/sector/save"
    |> request_input
         ~id:3
         ~params:{|{"name": "art", "description": "about art"}|}
    |> Jsonrpc.run ~services:Services.all
    |> Kohai_core.Eff.handle (module Handler)
    |> request_dump
  in
  let req4 =
    "kohai/sector/list"
    |> request_input ~id:4
    |> Jsonrpc.run ~services:Services.all
    |> Kohai_core.Eff.handle (module Handler)
    |> request_dump
  in
  List.iter print_endline [ req0; req1; req2; req3; req4 ];
  [%expect
    {|
    {id = 0; jsonrpc = "2.0"; result = "/supervised"}
    {id = 1; jsonrpc = "2.0";
      result =
       [{counter = 0; description = <null>; name = "art"};
        {counter = 0; description = "sector about cooking"; name = "cooking"};
        {counter = 0; description = "sector about programming";
          name = "programming"}]}
    {id = 2; jsonrpc = "2.0";
      result =
       [{counter = 0; description = <null>; name = "art"};
        {counter = 0; description = "sector about cooking"; name = "cooking"};
        {counter = 0; description = <null>; name = "learn"};
        {counter = 0; description = "sector about programming";
          name = "programming"}]}
    {id = 3; jsonrpc = "2.0";
      result =
       [{counter = 0; description = "about art"; name = "art"};
        {counter = 0; description = "sector about cooking"; name = "cooking"};
        {counter = 0; description = <null>; name = "learn"};
        {counter = 0; description = "sector about programming";
          name = "programming"}]}
    {id = 4; jsonrpc = "2.0";
      result =
       [{counter = 0; description = "about art"; name = "art"};
        {counter = 0; description = "sector about cooking"; name = "cooking"};
        {counter = 0; description = <null>; name = "learn"};
        {counter = 0; description = "sector about programming";
          name = "programming"}]}
    |}]
;;

let%expect_test "get one sector with file" =
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

      let now = Kohai_core.Datetime.unix
    end))
  in
  let _ =
    "kohai/supervision/set"
    |> request_input ~id:0 ~params:{|"/supervised"|}
    |> Jsonrpc.run ~services:Services.all
    |> Kohai_core.Eff.handle (module Handler)
  in
  let req1 =
    "kohai/sector/get"
    |> request_input ~id:1 ~params:{|"programming"|}
    |> Jsonrpc.run ~services:Services.all
    |> Kohai_core.Eff.handle (module Handler)
    |> request_dump
  in
  let req2 =
    "kohai/sector/get"
    |> request_input ~id:2 ~params:{|"cooking"|}
    |> Jsonrpc.run ~services:Services.all
    |> Kohai_core.Eff.handle (module Handler)
    |> request_dump
  in
  let req3 =
    "kohai/sector/get"
    |> request_input ~id:3 ~params:{|"art"|}
    |> Jsonrpc.run ~services:Services.all
    |> Kohai_core.Eff.handle (module Handler)
    |> request_dump
  in
  let req4 =
    "kohai/sector/get"
    |> request_input ~id:4 ~params:{|"not-exists"|}
    |> Jsonrpc.run ~services:Services.all
    |> Kohai_core.Eff.handle (module Handler)
    |> request_dump
  in
  let all =
    "kohai/sector/list"
    |> request_input ~id:5
    |> Jsonrpc.run ~services:Services.all
    |> Kohai_core.Eff.handle (module Handler)
    |> request_dump
  in
  List.iter print_endline [ req1; req2; req3; req4; all ];
  [%expect
    {|
    {id = 1; jsonrpc = "2.0";
      result =
       {counter = 0; description = "sector about programming";
         name = "programming"}}
    {id = 2; jsonrpc = "2.0";
      result =
       {counter = 0; description = "sector about cooking"; name = "cooking"}}
    {id = 3; jsonrpc = "2.0";
      result = {counter = 0; description = <null>; name = "art"}}
    {id = 4; jsonrpc = "2.0"; result = <null>}
    {id = 5; jsonrpc = "2.0";
      result =
       [{counter = 0; description = <null>; name = "art"};
        {counter = 0; description = "sector about cooking"; name = "cooking"};
        {counter = 0; description = "sector about programming";
          name = "programming"}]}
    |}]
;;
