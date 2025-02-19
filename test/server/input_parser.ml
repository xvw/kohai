let%expect_test "test-parser - 1" =
  let input = "Content-Length: 2\r\n\r\naa" in
  let source = Eio.Flow.string_source input in
  let () =
    match
      Eio.Buf_read.parse ~max_size:1024 Kohai_server.Server.input_parser source
    with
    | Ok s -> print_endline ("Ok: " ^ s)
    | Error (`Msg err) -> print_endline ("Error: " ^ err)
  in
  ();
  [%expect {| Ok: aa |}]
;;

let%expect_test "test-parser - 2" =
  let obj =
    Kohai_server.Error.parse_error ~body:"{foo}" ()
    |> Kohai_server.Error.to_rensai
    |> Rensai.Json.to_yojson
    |> Yojson.Safe.to_string
  in
  let len = String.length obj in
  let input = Format.asprintf "Content-Length: %d\r\n\r\n%s" len obj in
  let source = Eio.Flow.string_source input in
  let () =
    match
      Eio.Buf_read.parse ~max_size:1024 Kohai_server.Server.input_parser source
    with
    | Ok s -> print_endline ("Ok: " ^ s)
    | Error (`Msg err) -> print_endline ("Error: " ^ err)
  in
  ();
  [%expect
    {| Ok: {"error":{"code":-32700,"data":null,"input":"{foo}","message":"Parse error"},"id":null,"jsonrpc":"2.0"} |}]
;;

let%expect_test "test-parser - 3" =
  let obj =
    Kohai_server.Error.parse_error ~body:"{foo}" ()
    |> Kohai_server.Error.to_rensai
    |> Rensai.Json.to_yojson
    |> Yojson.Safe.to_string
  in
  let len = String.length obj in
  let input = Format.asprintf "Content-Length: %d\r\n\r\n%s    " len obj in
  let source = Eio.Flow.string_source input in
  let () =
    match
      Eio.Buf_read.parse ~max_size:1024 Kohai_server.Server.input_parser source
    with
    | Ok s -> print_endline ("Ok: " ^ s)
    | Error (`Msg err) -> print_endline ("Error: " ^ err)
  in
  ();
  [%expect {| Error: Unexpected data after parsing (at offset 126) |}]
;;

let%expect_test "test-parser - 4" =
  let input = Format.asprintf "Content" in
  let source = Eio.Flow.string_source input in
  let () =
    match
      Eio.Buf_read.parse ~max_size:1024 Kohai_server.Server.input_parser source
    with
    | Ok s -> print_endline ("Ok: " ^ s)
    | Error (`Msg err) -> print_endline ("Error: " ^ err)
  in
  ();
  [%expect {| Error: Unexpected end-of-file at offset 7 |}]
;;

let%expect_test "test-parser - 5" =
  let input = Format.asprintf "Content-Length: 1000\r\n\r\nfoo" in
  let source = Eio.Flow.string_source input in
  let () =
    match
      Eio.Buf_read.parse ~max_size:1024 Kohai_server.Server.input_parser source
    with
    | Ok s -> print_endline ("Ok: " ^ s)
    | Error (`Msg err) -> print_endline ("Error: " ^ err)
  in
  ();
  [%expect {| Error: Unexpected end-of-file at offset 27 |}]
;;
