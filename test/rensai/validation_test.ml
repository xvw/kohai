let dump_error err =
  err |> Format.asprintf "%a" Rensai_fmt.pp_value_error |> print_endline
;;

let dump_value pp value = value |> Format.asprintf "%a" pp |> print_endline

let dump pp = function
  | Ok x -> dump_value pp x
  | Error err -> dump_error err
;;

let pp_ok ppf _ = Format.fprintf ppf "ok"

open Rensai

let%expect_test "validate a null value" =
  let expr = Ast.null () in
  let check = Validation.null in
  expr |> check |> dump pp_ok;
  [%expect {| ok |}]
;;

let%expect_test "validate a non-null value - 1" =
  let expr = Ast.unit () in
  let check = Validation.null in
  expr |> check |> dump pp_ok;
  [%expect
    {| Unexpected_kind {expected = "null"; given = "unit"; value = <unit>} |}]
;;

let%expect_test "validate a non-null value - 2" =
  let expr =
    Ast.(
      hlist
        [ int 12
        ; string "foo"
        ; hlist [ int64 1344L; bool true ]
        ; quad' int int string string 1 2 "foo" "bar"
        ])
  in
  let check = Validation.null in
  expr |> check |> dump pp_ok;
  [%expect
    {|
    Unexpected_kind {expected = "null";
                     given =
                      "[int & string & [int64 & bool] & (int *  (int *  (string *  string)))]";
                     value = [12; "foo"; [1344L; true]; (1, (2, ("foo", "bar")))]}
    |}]
;;

let%expect_test "validate an unit value" =
  let expr = Ast.unit () in
  let check = Validation.unit in
  expr |> check |> dump pp_ok;
  [%expect {| ok |}]
;;

let%expect_test "validate an non-unit value - 1" =
  let expr = Ast.null () in
  let check = Validation.unit in
  expr |> check |> dump pp_ok;
  [%expect
    {| Unexpected_kind {expected = "unit"; given = "null"; value = <null>} |}]
;;

let%expect_test "validate a non-unit value - 2" =
  let expr =
    Ast.(
      hlist
        [ int 12
        ; string "foo"
        ; hlist [ int64 1344L; bool true ]
        ; quad' int int string string 1 2 "foo" "bar"
        ])
  in
  let check = Validation.unit in
  expr |> check |> dump pp_ok;
  [%expect
    {|
    Unexpected_kind {expected = "unit";
                     given =
                      "[int & string & [int64 & bool] & (int *  (int *  (string *  string)))]";
                     value = [12; "foo"; [1344L; true]; (1, (2, ("foo", "bar")))]}
    |}]
;;

let%expect_test "validate an unitish value - 1" =
  let expr = Ast.unit () in
  let check = Validation.unitish in
  expr |> check |> dump pp_ok;
  [%expect {| ok |}]
;;

let%expect_test "validate an unitish value - 2" =
  let expr = Ast.null () in
  let check = Validation.unitish in
  expr |> check |> dump pp_ok;
  [%expect {| ok |}]
;;

let%expect_test "validate a bool value - 1" =
  let expr = Ast.bool true in
  let check = Validation.bool in
  expr |> check |> dump pp_ok;
  [%expect {| ok |}]
;;

let%expect_test "validate a bool value - 2" =
  let expr = Ast.bool false in
  let check = Validation.bool in
  expr |> check |> dump pp_ok;
  [%expect {| ok |}]
;;

let%expect_test "validate a non-bool value - 1" =
  let expr =
    Ast.(
      hlist
        [ int 12
        ; string "bar"
        ; hlist [ int64 1344L; bool true ]
        ; quad' int int string string 1 2 "foo" "bar"
        ])
  in
  let check = Validation.bool in
  expr |> check |> dump pp_ok;
  [%expect
    {|
    Unexpected_kind {expected = "bool";
                     given =
                      "[int & string & [int64 & bool] & (int *  (int *  (string *  string)))]";
                     value = [12; "bar"; [1344L; true]; (1, (2, ("foo", "bar")))]}
    |}]
;;

let%expect_test "validate a char value" =
  let expr = Ast.char 'x' in
  let check = Validation.char in
  expr |> check |> dump pp_ok;
  [%expect {| ok |}]
;;

let%expect_test "validate a char value using a string value" =
  let expr = Ast.string "x" in
  let check = Validation.char in
  expr |> check |> dump pp_ok;
  [%expect {| ok |}]
;;

let%expect_test "validate a char value using an invalid string value" =
  let expr = Ast.string "xo" in
  let check = Validation.char in
  expr |> check |> dump pp_ok;
  [%expect
    {| Unexpected_kind {expected = "char"; given = "string"; value = "xo"} |}]
;;

let%expect_test "validate a char value using an invalid value" =
  let expr = Ast.int 45 in
  let check = Validation.char in
  expr |> check |> dump pp_ok;
  [%expect {| Unexpected_kind {expected = "char"; given = "int"; value = 45} |}]
;;

let%expect_test "validate a char value using a string value in strict mode" =
  let expr = Ast.string "x" in
  let check = Validation.char ~strict:true in
  expr |> check |> dump pp_ok;
  [%expect
    {| Unexpected_kind {expected = "char"; given = "string"; value = "x"} |}]
;;

let%expect_test "validate an int value" =
  let expr = Ast.int 42 in
  let check = Validation.int in
  expr |> check |> dump pp_ok;
  [%expect {| ok |}]
;;

let%expect_test "validate an int value using a string" =
  let expr = Ast.string "42" in
  let check = Validation.int in
  expr |> check |> dump pp_ok;
  [%expect {| ok |}]
;;

let%expect_test "validate an int value using a string in strict mode" =
  let expr = Ast.string "42" in
  let check = Validation.int ~strict:true in
  expr |> check |> dump pp_ok;
  [%expect
    {| Unexpected_kind {expected = "int"; given = "string"; value = "42"} |}]
;;

let%expect_test "validate an int value using an int32" =
  let expr = Ast.int32 42l in
  let check = Validation.int in
  expr |> check |> dump pp_ok;
  [%expect {| ok |}]
;;

let%expect_test "validate an int value using an int32 in strict mode" =
  let expr = Ast.int32 42l in
  let check = Validation.int ~strict:true in
  expr |> check |> dump pp_ok;
  [%expect
    {| Unexpected_kind {expected = "int"; given = "int32"; value = 42l} |}]
;;

let%expect_test "validate an int value using an int64" =
  let expr = Ast.int64 42L in
  let check = Validation.int in
  expr |> check |> dump pp_ok;
  [%expect {| ok |}]
;;

let%expect_test "validate an int value using an invalid int64" =
  let expr = Ast.int64 Int64.max_int in
  let check = Validation.int in
  expr |> check |> dump pp_ok;
  [%expect
    {|
    Unexpected_kind {expected = "int"; given = "int64";
                     value = 9223372036854775807L}
    |}]
;;

let%expect_test "validate an int value using an int64 in strict mode" =
  let expr = Ast.int64 42L in
  let check = Validation.int ~strict:true in
  expr |> check |> dump pp_ok;
  [%expect
    {| Unexpected_kind {expected = "int"; given = "int64"; value = 42L} |}]
;;

let%expect_test "validate an int value using a float" =
  let expr = Ast.float 42. in
  let check = Validation.int in
  expr |> check |> dump pp_ok;
  [%expect {| ok |}]
;;

let%expect_test "validate an int value using an invalid float" =
  let expr = Ast.float 42.32 in
  let check = Validation.int in
  expr |> check |> dump pp_ok;
  [%expect {| Unexpected_kind {expected = "int"; given = "float"; value = 42.32} |}]
;;

let%expect_test "validate an int value using a float in strict mode" =
  let expr = Ast.float 42. in
  let check = Validation.int ~strict:true in
  expr |> check |> dump pp_ok;
  [%expect
    {| Unexpected_kind {expected = "int"; given = "float"; value = 42.} |}]
;;
