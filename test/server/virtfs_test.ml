open Kohai_core

let a_fs =
  let open Virtfs in
  from_list
    [ dir
        "foo"
        [ dir "bar" [ file ~content:"foobar" "foobar" ]
        ; dir "baz" [ file ~content:"a textual file" "notes.txt" ]
        ]
    ; dir "tmp" []
    ]
;;

let%expect_test "cat - with a file" =
  let result = Virtfs.cat a_fs Path.(root / "foo" / "bar" / "foobar") in
  print_endline result;
  [%expect {| foobar |}]
;;

let%expect_test "cat - with an other file" =
  let result = Virtfs.cat a_fs Path.(root / "foo" / "baz" / "notes.txt") in
  print_endline result;
  [%expect {| a textual file |}]
;;

let%expect_test "cat - with a directory" =
  let result = Virtfs.cat a_fs Path.(root / "foo" / "tmp") in
  print_endline result;
  [%expect {| cat: /foo/tmp: No such file or directory |}]
;;

let%expect_test "cat - with an inexistant target" =
  let result = Virtfs.cat a_fs Path.(root / "kohai" / "tmp") in
  print_endline result;
  [%expect {| cat: /kohai/tmp: No such file or directory |}]
;;
