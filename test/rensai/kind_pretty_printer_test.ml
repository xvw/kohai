let dump rensai_expr =
  rensai_expr
  |> Rensai.Kind.classify
  |> Format.asprintf "%a" Rensai.Kind.pp
  |> print_endline
;;

open Rensai.Ast

let%expect_test "pretty print kind of null" =
  let expr = null () in
  dump expr;
  [%expect {| null |}]
;;

let%expect_test "pretty print kind of unit" =
  let expr = unit () in
  dump expr;
  [%expect {| unit |}]
;;

let%expect_test "pretty print kind of false" =
  let expr = bool false in
  dump expr;
  [%expect {| bool |}]
;;

let%expect_test "pretty print kind of true" =
  let expr = bool true in
  dump expr;
  [%expect {| bool |}]
;;

let%expect_test "pretty print kind of a char" =
  let expr = char 'x' in
  dump expr;
  [%expect {| char |}]
;;

let%expect_test "pretty print kind of an int" =
  let expr = int 42 in
  dump expr;
  [%expect {| int |}]
;;

let%expect_test "pretty print kind of an int32" =
  let expr = int32 42l in
  dump expr;
  [%expect {| int32 |}]
;;

let%expect_test "pretty print kind of an int64" =
  let expr = int64 42L in
  dump expr;
  [%expect {| int64 |}]
;;

let%expect_test "pretty print kind of a float" =
  let expr = float 42.12 in
  dump expr;
  [%expect {| float |}]
;;

let%expect_test "pretty print kind of a string" =
  let expr = string "foo" in
  dump expr;
  [%expect {| string |}]
;;

let%expect_test "pretty print kind of a pair" =
  let expr = pair' int string 10 "foo" in
  dump expr;
  [%expect {| (int, string) |}]
;;

let%expect_test "pretty print kind of an other pair" =
  let expr = quad' int string bool unit 10 "foo" true () in
  dump expr;
  [%expect {| (int, (string, (bool, unit))) |}]
;;

let%expect_test "pretty print kind a regular list" =
  let expr = list int [ 1; 2; 3; 4 ] in
  dump expr;
  [%expect {| list<int> |}]
;;

let%expect_test "pretty print kind an other regular list" =
  let expr = hlist [ int 1; int 2; int 3 ] in
  dump expr;
  [%expect {| list<int> |}]
;;

let%expect_test "pretty print kind an irregular list" =
  let expr = hlist [ int 1; int 2; int 3; float 32.5; int64 4L; bool false ] in
  dump expr;
  [%expect {|
    list<int & float & int64 &
     bool>
    |}]
;;

let%expect_test "pretty print kind a constructor" =
  let expr =
    constr
      (function
        | `Foo x -> "foo", int x)
      (`Foo 10)
  in
  dump expr;
  [%expect {| foo(int) |}]
;;

let%expect_test "Avoid everything using the sad record type!" =
  let expr =
    record
      [ "age", option int (Some 42)
      ; "activated", either string int (Left "foo")
      ; ( "sub"
        , record
            [ "is_valid", result bool bool (Error false)
            ; ( "a_long_string"
              , string
                  "Vivamus quis felis sit amet nunc pretium aliquet. \
                   Suspendisse a magna ut nisl sodales blandit sed et mi. \
                   Quisque fermentum hendrerit lectus ac pulvinar. Duis \
                   euismod magna et magna convallis viverra. Pellentesque \
                   laoreet luctus pellentesque. Sed sagittis viverra leo, quis \
                   auctor nisi cursus ac. Nam cursus urna et tincidunt \
                   gravida. Ut eget ipsum in massa sagittis vestibulum eget at \
                   est. Vestibulum bibendum mattis quam, sit amet tincidunt \
                   ligula. Aliquam at tempus augue. Aenean ac est urna. Nullam \
                   iaculis, dolor i" )
            ; ( "another_random_list"
              , list
                  (fun x ->
                     record
                       [ "x", string x
                       ; "up", string (String.uppercase_ascii x)
                       ])
                  (List.init 43 (fun i -> String.make i 'a')) )
            ] )
      ; "a_random_list", list int (List.init 320 Fun.id)
      ]
  in
  dump expr;
  [%expect {| ?record |}]
;;
