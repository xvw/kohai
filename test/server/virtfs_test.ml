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
  let result = Virtfs.cat a_fs Path.(root / "tmp") in
  print_endline result;
  [%expect {| cat: /tmp: Is a directory |}]
;;

let%expect_test "cat - with an inexistant target" =
  let result = Virtfs.cat a_fs Path.(root / "kohai" / "tmp") in
  print_endline result;
  [%expect {| cat: /kohai/tmp: No such file or directory |}]
;;

let%expect_test "create-dir - 1" =
  let fs =
    Virtfs.update
      a_fs
      Path.(root / "xvw" / "lol" / "foobar")
      (fun ~target ?previous:_ () -> Some (Virtfs.dir target []))
  in
  let xvw = Virtfs.cat fs Path.(root / "xvw") in
  let lol = Virtfs.cat fs Path.(root / "xvw" / "lol") in
  let foobar = Virtfs.cat fs Path.(root / "xvw" / "lol" / "foobar") in
  List.iter print_endline [ xvw; lol; foobar ];
  [%expect
    {|
    cat: /xvw: Is a directory
    cat: /xvw/lol: Is a directory
    cat: /xvw/lol/foobar: Is a directory
    |}]
;;
