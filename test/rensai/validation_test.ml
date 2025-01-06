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

let%expect_test "validate a bool value in strict mode - 1" =
  let expr = Ast.bool true in
  let check = Validation.bool ~strict:true in
  expr |> check |> dump pp_ok;
  [%expect {| ok |}]
;;

let%expect_test "validate a bool value in strict mode - 2" =
  let expr = Ast.bool false in
  let check = Validation.bool ~strict:true in
  expr |> check |> dump pp_ok;
  [%expect {| ok |}]
;;

let%expect_test "validate a bool value in strict mode - 1" =
  let expr = Ast.bool true in
  let check = Validation.bool ~strict:true in
  expr |> check |> dump pp_ok;
  [%expect {| ok |}]
;;

let%expect_test "validate a bool value using string - 1" =
  let expr = Ast.string "TrUe" in
  let check = Validation.bool in
  expr |> check |> dump pp_ok;
  [%expect {| ok |}]
;;

let%expect_test "validate a bool value using string - 2" =
  let expr = Ast.string "  false " in
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

let%expect_test "validate a char value in strict mode" =
  let expr = Ast.char 'x' in
  let check = Validation.char ~strict:true in
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

let%expect_test "validate an int value in strict mode" =
  let expr = Ast.int 42 in
  let check = Validation.int ~strict:true in
  expr |> check |> dump pp_ok;
  [%expect {| ok |}]
;;

let%expect_test "validate an int value using a string" =
  let expr = Ast.string "42" in
  let check = Validation.int in
  expr |> check |> dump pp_ok;
  [%expect {| ok |}]
;;

let%expect_test "validate an int value using an invalid string - 1" =
  let expr = Ast.string "foo" in
  let check = Validation.int in
  expr |> check |> dump pp_ok;
  [%expect
    {| Unexpected_kind {expected = "int"; given = "string"; value = "foo"} |}]
;;

let%expect_test "validate an int value using an invalid string - 2" =
  let expr = Ast.string "33sasa" in
  let check = Validation.int in
  expr |> check |> dump pp_ok;
  [%expect
    {| Unexpected_kind {expected = "int"; given = "string"; value = "33sasa"} |}]
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
  [%expect
    {| Unexpected_kind {expected = "int"; given = "float"; value = 42.32} |}]
;;

let%expect_test "validate an int value using a float in strict mode" =
  let expr = Ast.float 42. in
  let check = Validation.int ~strict:true in
  expr |> check |> dump pp_ok;
  [%expect
    {| Unexpected_kind {expected = "int"; given = "float"; value = 42.} |}]
;;

let%expect_test "validate an int32 value" =
  let expr = Ast.int32 42l in
  let check = Validation.int32 in
  expr |> check |> dump pp_ok;
  [%expect {| ok |}]
;;

let%expect_test "validate an int32 value in strict mode" =
  let expr = Ast.int32 42l in
  let check = Validation.int32 ~strict:true in
  expr |> check |> dump pp_ok;
  [%expect {| ok |}]
;;

let%expect_test "validate an int32 value using an int" =
  let expr = Ast.int 42 in
  let check = Validation.int32 in
  expr |> check |> dump pp_ok;
  [%expect {| ok |}]
;;

let%expect_test "validate an int32 value using an int in strict mode" =
  let expr = Ast.int 42 in
  let check = Validation.int32 ~strict:true in
  expr |> check |> dump pp_ok;
  [%expect
    {| Unexpected_kind {expected = "int32"; given = "int"; value = 42} |}]
;;

let%expect_test "validate an int32 value using an int64" =
  let expr = Ast.int64 42L in
  let check = Validation.int32 in
  expr |> check |> dump pp_ok;
  [%expect {| ok |}]
;;

let%expect_test "validate an int32 value using an invalid int64" =
  let expr = Ast.int64 Int64.max_int in
  let check = Validation.int32 in
  expr |> check |> dump pp_ok;
  [%expect
    {|
    Unexpected_kind {expected = "int32"; given = "int64";
                     value = 9223372036854775807L}
    |}]
;;

let%expect_test "validate an int32 value using an int64 in strict mode" =
  let expr = Ast.int64 42L in
  let check = Validation.int32 ~strict:true in
  expr |> check |> dump pp_ok;
  [%expect
    {| Unexpected_kind {expected = "int32"; given = "int64"; value = 42L} |}]
;;

let%expect_test "validate an int32 value using a float" =
  let expr = Ast.float 42. in
  let check = Validation.int32 in
  expr |> check |> dump pp_ok;
  [%expect {| ok |}]
;;

let%expect_test "validate an int32 value using a float in strict mode" =
  let expr = Ast.float 42. in
  let check = Validation.int32 ~strict:true in
  expr |> check |> dump pp_ok;
  [%expect
    {| Unexpected_kind {expected = "int32"; given = "float"; value = 42.} |}]
;;

let%expect_test "validate an int32 value using an invalid float" =
  let expr = Ast.float 3.14 in
  let check = Validation.int32 in
  expr |> check |> dump pp_ok;
  [%expect
    {| Unexpected_kind {expected = "int32"; given = "float"; value = 3.14} |}]
;;

let%expect_test "validate an int32 value using a string" =
  let expr = Ast.string "42" in
  let check = Validation.int32 in
  expr |> check |> dump pp_ok;
  [%expect {| ok |}]
;;

let%expect_test "validate an int32 value using a string in strict mode" =
  let expr = Ast.string "42" in
  let check = Validation.int32 ~strict:true in
  expr |> check |> dump pp_ok;
  [%expect
    {| Unexpected_kind {expected = "int32"; given = "string"; value = "42"} |}]
;;

let%expect_test "validate an int32 value using an invalid string" =
  let expr = Ast.string "hello" in
  let check = Validation.int32 in
  expr |> check |> dump pp_ok;
  [%expect
    {| Unexpected_kind {expected = "int32"; given = "string"; value = "hello"} |}]
;;

let%expect_test "validate an int64 value" =
  let expr = Ast.int64 42L in
  let check = Validation.int64 in
  expr |> check |> dump pp_ok;
  [%expect {| ok |}]
;;

let%expect_test "validate an int64 value in strict mode" =
  let expr = Ast.int64 42L in
  let check = Validation.int64 ~strict:true in
  expr |> check |> dump pp_ok;
  [%expect {| ok |}]
;;

let%expect_test "validate an int value as an int64" =
  let expr = Ast.int 42 in
  let check = Validation.int64 in
  expr |> check |> dump pp_ok;
  [%expect {| ok |}]
;;

let%expect_test "validate an int value as an int64 in strict mode" =
  let expr = Ast.int 42 in
  let check = Validation.int64 ~strict:true in
  expr |> check |> dump pp_ok;
  [%expect
    {| Unexpected_kind {expected = "int64"; given = "int"; value = 42} |}]
;;

let%expect_test "validate an int32 value as an int64" =
  let expr = Ast.int32 42l in
  let check = Validation.int64 in
  expr |> check |> dump pp_ok;
  [%expect {| ok |}]
;;

let%expect_test "validate an int32 value as an int64 in strict mode" =
  let expr = Ast.int32 42l in
  let check = Validation.int64 ~strict:true in
  expr |> check |> dump pp_ok;
  [%expect
    {| Unexpected_kind {expected = "int64"; given = "int32"; value = 42l} |}]
;;

let%expect_test "validate a float value as an int64" =
  let expr = Ast.float 42.0 in
  let check = Validation.int64 in
  expr |> check |> dump pp_ok;
  [%expect {| ok |}]
;;

let%expect_test "validate an invalid float value as an int64" =
  let expr = Ast.float 42.22 in
  let check = Validation.int64 in
  expr |> check |> dump pp_ok;
  [%expect
    {| Unexpected_kind {expected = "int64"; given = "float"; value = 42.22} |}]
;;

let%expect_test "validate a float value as an int64 in strict mode" =
  let expr = Ast.float 42.0 in
  let check = Validation.int64 ~strict:true in
  expr |> check |> dump pp_ok;
  [%expect
    {| Unexpected_kind {expected = "int64"; given = "float"; value = 42.} |}]
;;

let%expect_test "validate a float value as an int64" =
  let expr = Ast.float 42.0 in
  let check = Validation.int64 in
  expr |> check |> dump pp_ok;
  [%expect {| ok |}]
;;

let%expect_test "validate a string value as an int64" =
  let expr = Ast.string "345678909" in
  let check = Validation.int64 in
  expr |> check |> dump pp_ok;
  [%expect {| ok |}]
;;

let%expect_test "validate a string value as an invalid int64" =
  let expr = Ast.string "kkk345678909" in
  let check = Validation.int64 in
  expr |> check |> dump pp_ok;
  [%expect
    {|
    Unexpected_kind {expected = "int64"; given = "string";
                     value = "kkk345678909"}
    |}]
;;

let%expect_test "validate a string value as an int64 in strict mode" =
  let expr = Ast.string "345678909" in
  let check = Validation.int64 ~strict:true in
  expr |> check |> dump pp_ok;
  [%expect
    {| Unexpected_kind {expected = "int64"; given = "string"; value = "345678909"} |}]
;;

let%expect_test "validate a float" =
  let expr = Ast.float 42.0 in
  let check = Validation.float in
  expr |> check |> dump pp_ok;
  [%expect {| ok |}]
;;

let%expect_test "validate a float in strict mode" =
  let expr = Ast.float 42.0 in
  let check = Validation.float ~strict:true in
  expr |> check |> dump pp_ok;
  [%expect {| ok |}]
;;

let%expect_test "validate a float using an int" =
  let expr = Ast.int 42 in
  let check = Validation.float in
  expr |> check |> dump pp_ok;
  [%expect {| ok |}]
;;

let%expect_test "validate a float using an int in strict mode" =
  let expr = Ast.int 42 in
  let check = Validation.float ~strict:true in
  expr |> check |> dump pp_ok;
  [%expect
    {| Unexpected_kind {expected = "float"; given = "int"; value = 42} |}]
;;

let%expect_test "validate a float using an int32" =
  let expr = Ast.int32 42l in
  let check = Validation.float in
  expr |> check |> dump pp_ok;
  [%expect {| ok |}]
;;

let%expect_test "validate a float using an int32 in strict mode" =
  let expr = Ast.int32 42l in
  let check = Validation.float ~strict:true in
  expr |> check |> dump pp_ok;
  [%expect
    {| Unexpected_kind {expected = "float"; given = "int32"; value = 42l} |}]
;;

let%expect_test "validate a float using an int64" =
  let expr = Ast.int64 42L in
  let check = Validation.float in
  expr |> check |> dump pp_ok;
  [%expect {| ok |}]
;;

let%expect_test "validate a float using an int64 in strict mode" =
  let expr = Ast.int64 42L in
  let check = Validation.float ~strict:true in
  expr |> check |> dump pp_ok;
  [%expect
    {| Unexpected_kind {expected = "float"; given = "int64"; value = 42L} |}]
;;

let%expect_test "validate a float using a string - 1" =
  let expr = Ast.string "3.14" in
  let check = Validation.float in
  expr |> check |> dump pp_ok;
  [%expect {| ok |}]
;;

let%expect_test "validate a float using a string - 2" =
  let expr = Ast.string "3" in
  let check = Validation.float in
  expr |> check |> dump pp_ok;
  [%expect {| ok |}]
;;

let%expect_test "validate a float using an invalid string" =
  let expr = Ast.string "foo" in
  let check = Validation.float in
  expr |> check |> dump pp_ok;
  [%expect
    {| Unexpected_kind {expected = "float"; given = "string"; value = "foo"} |}]
;;

let%expect_test "validate a float using a string in strict mode" =
  let expr = Ast.string "3.14" in
  let check = Validation.float ~strict:true in
  expr |> check |> dump pp_ok;
  [%expect
    {| Unexpected_kind {expected = "float"; given = "string"; value = "3.14"} |}]
;;

let%expect_test "validate an integer - 1" =
  let expr = Ast.int 1 in
  let check = Validation.integer ~strict:true in
  expr |> check |> dump pp_ok;
  [%expect {| ok |}]
;;

let%expect_test "validate an integer - 2" =
  let expr = Ast.int32 1l in
  let check = Validation.integer ~strict:true in
  expr |> check |> dump pp_ok;
  [%expect {| ok |}]
;;

let%expect_test "validate an integer - 3" =
  let expr = Ast.int64 2L in
  let check = Validation.integer ~strict:true in
  expr |> check |> dump pp_ok;
  [%expect {| ok |}]
;;

let%expect_test "validate an integer with a float" =
  let expr = Ast.float 2.33 in
  let check = Validation.integer ~strict:true in
  expr |> check |> dump pp_ok;
  [%expect
    {|
    Unexpected_kind {expected = "int | int32 | int64"; given = "float";
                     value = 2.33}
    |}]
;;

let%expect_test "validate an integer with a float in non-strict mode" =
  let expr = Ast.float 2.0 in
  let check = Validation.integer in
  expr |> check |> dump pp_ok;
  [%expect {| ok |}]
;;

let%expect_test "validate a number - 1" =
  let expr = Ast.int 1 in
  let check = Validation.number ~strict:true in
  expr |> check |> dump pp_ok;
  [%expect {| ok |}]
;;

let%expect_test "validate a number - 2" =
  let expr = Ast.int32 1l in
  let check = Validation.number ~strict:true in
  expr |> check |> dump pp_ok;
  [%expect {| ok |}]
;;

let%expect_test "validate a number - 3" =
  let expr = Ast.int64 1L in
  let check = Validation.number ~strict:true in
  expr |> check |> dump pp_ok;
  [%expect {| ok |}]
;;

let%expect_test "validate a number - 4" =
  let expr = Ast.float 3.14 in
  let check = Validation.number ~strict:true in
  expr |> check |> dump pp_ok;
  [%expect {| ok |}]
;;

let%expect_test "validate an invalid number" =
  let expr = Ast.string "Foo" in
  let check = Validation.number in
  expr |> check |> dump pp_ok;
  [%expect {|
    Unexpected_kind {expected = "int | int32 | int64 | float"; given = "string";
                     value = "Foo"}
    |}]
;;