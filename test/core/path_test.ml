open Core

let dump_str s = s |> Fmt.str "%a" Fmt.Dump.string |> print_endline
let dump_path p = p |> Path.to_string |> dump_str
let dump_opt o = o |> Fmt.str "%a" Fmt.Dump.(option string) |> print_endline

let%expect_test "path building - 1" =
  let p = Path.(pwd) in
  dump_path p;
  [%expect {| "./" |}]
;;

let%expect_test "path building - 2" =
  let p = Path.(root) in
  dump_path p;
  [%expect {| "/" |}]
;;

let%expect_test "path building - 3" =
  let p = Path.(pwd / "foo") in
  dump_path p;
  [%expect {| "./foo" |}]
;;

let%expect_test "path building - 4" =
  let p = Path.(pwd / "foo" / "bar" / "baz") in
  dump_path p;
  [%expect {| "./foo/bar/baz" |}]
;;

let%expect_test "path building - 5" =
  let p = Path.(root / "foo" / "bar" / "baz") in
  dump_path p;
  [%expect {| "/foo/bar/baz" |}]
;;

let%expect_test "extension - 1" =
  let s = Path.(pwd) |> Path.extension in
  dump_str s;
  [%expect {| "" |}]
;;

let%expect_test "extension - 2" =
  let s = Path.(root) |> Path.extension in
  dump_str s;
  [%expect {| "" |}]
;;

let%expect_test "extension - 3" =
  let s = Path.(pwd / "index.ml") |> Path.extension in
  dump_str s;
  [%expect {| ".ml" |}]
;;

let%expect_test "extension - 5" =
  let s = Path.(pwd / "index/module.mli") |> Path.extension in
  dump_str s;
  [%expect {| ".mli" |}]
;;

let%expect_test "extension_opt - 1" =
  let s = Path.(pwd) |> Path.extension_opt in
  dump_opt s;
  [%expect {| None |}]
;;

let%expect_test "extension_opt - 2" =
  let s = Path.(root) |> Path.extension_opt in
  dump_opt s;
  [%expect {| None |}]
;;

let%expect_test "extension_opt - 3" =
  let s = Path.(pwd / "index.ml") |> Path.extension_opt in
  dump_opt s;
  [%expect {| Some ".ml" |}]
;;

let%expect_test "extension_opt - 5" =
  let s = Path.(pwd / "index/module.mli") |> Path.extension_opt in
  dump_opt s;
  [%expect {| Some ".mli" |}]
;;
