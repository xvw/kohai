open Kohai_model

let dump ?(should_fail = false) = function
  | Ok url when not should_fail ->
    url |> Format.asprintf "OK: %a" Rensai.Lang.pp |> print_endline
  | Ok url ->
    url
    |> Format.asprintf "ERROR: %a (should failed)" Rensai.Lang.pp
    |> print_endline
  | Error err ->
    let suff = if should_fail then "OK" else "ERROR" in
    err
    |> Format.asprintf
         "%s: %a (should failed)"
         suff
         Rensai.Validation.pp_value_error
    |> print_endline
;;

let%expect_test "from_string - 1" =
  let url = "http://xvw.lol" in
  url |> Url.from_string |> Result.map Url.to_rensai |> dump;
  [%expect
    {|
    OK: <host: "xvw.lol"; path: "/"; port: null; query: []; repr: "xvw.lol/";
          scheme: "http";
          uri: <host: "xvw.lol"; path: ""; port: null; query: []; scheme: "http">;
          url: "http://xvw.lol">
    |}]
;;

let%expect_test "from_string - 2" =
  let url = "http://xvw.lol/foo/bar" in
  url |> Url.from_string |> Result.map Url.to_rensai |> dump;
  [%expect
    {|
    OK: <host: "xvw.lol"; path: "/foo/bar"; port: null; query: [];
          repr: "xvw.lol/foo/bar"; scheme: "http";
          uri:
           <host: "xvw.lol"; path: "/foo/bar"; port: null; query: [];
             scheme: "http">;
          url: "http://xvw.lol/foo/bar">
    |}]
;;

let%expect_test "from_string - 3" =
  let url = "http://xvw.lol/foo/bar?foo=bar,baz&bar=foo" in
  url |> Url.from_string |> Result.map Url.to_rensai |> dump;
  [%expect
    {|
    OK: <host: "xvw.lol"; path: "/foo/bar"; port: null;
          query:
           [<key: "bar"; value: ["foo"]>, <key: "foo"; value: ["bar", "baz"]>];
          repr: "xvw.lol/foo/bar"; scheme: "http";
          uri:
           <host: "xvw.lol"; path: "/foo/bar"; port: null;
             query: [("foo", ["bar", "baz"]), ("bar", ["foo"])]; scheme: "http">;
          url: "http://xvw.lol/foo/bar?foo=bar,baz&bar=foo">
    |}]
;;

let%expect_test "from_string - 4" =
  let url = "http://xvw.lol:8888/foo/bar?foo=bar,baz&bar=foo" in
  url |> Url.from_string |> Result.map Url.to_rensai |> dump;
  [%expect
    {|
    OK: <host: "xvw.lol"; path: "/foo/bar"; port: 8888;
          query:
           [<key: "bar"; value: ["foo"]>, <key: "foo"; value: ["bar", "baz"]>];
          repr: "xvw.lol/foo/bar"; scheme: "http";
          uri:
           <host: "xvw.lol"; path: "/foo/bar"; port: 8888;
             query: [("foo", ["bar", "baz"]), ("bar", ["foo"])]; scheme: "http">;
          url: "http://xvw.lol:8888/foo/bar?foo=bar,baz&bar=foo">
    |}]
;;

let%expect_test "from_string - 5" =
  let url = "www.google.org" in
  url |> Url.from_string |> Result.map Url.to_rensai |> dump;
  [%expect
    {|
    ERROR: {message: "unexpected record";
            where:
             [{message: "missing field";
               field: "scheme"};
              {message: "missing field";
               field: "host"}];
            value:
             {host = <null>; path = "www.google.org"; port = <null>; query = [];
               scheme = <null>}} (should failed)
    |}]
;;
