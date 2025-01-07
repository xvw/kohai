let print pp value =
  value
  |> Format.asprintf "%a" (Rensai.Validation.pp_checked pp)
  |> print_endline
;;

let pp_ok ppf _ = Format.fprintf ppf "ok"
let dump_null = Fmt.any "null"
let dump_unit = Fmt.any "null"
let dump_bool = Fmt.bool
let dump_char = Fmt.char
let dump_int = Fmt.int
let dump_int32 = Fmt.int32
let dump_int64 = Fmt.int64
let dump_float = Fmt.float
let dump_string = Fmt.Dump.string
let dump_pair a b = Fmt.Dump.pair a b

let dump_triple a b c =
  let p = Fmt.Dump.(pair a (pair b c)) in
  Fmt.using (fun (x, y, z) -> x, (y, z)) p
;;

let dump_quad a b c d =
  let p = Fmt.Dump.(pair a (pair b (pair c d))) in
  Fmt.using (fun (w, x, y, z) -> w, (x, (y, z))) p
;;

open Rensai

let%expect_test "validate a null value" =
  let expr = Ast.null () in
  let check = Validation.null in
  expr |> check |> print dump_null;
  [%expect {| null |}]
;;

let%expect_test "validate a non-null value - 1" =
  let expr = Ast.unit () in
  let check = Validation.null in
  expr |> check |> print dump_null;
  [%expect
    {|
    {message: "unexpected kind";
     expected: null;
     given: unit;
     value: <unit>}
    |}]
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
  expr |> check |> print dump_null;
  [%expect
    {|
    {message: "unexpected kind";
     expected: null;
     given: list<int & string & list<int64 & bool> &
       (int, (int, (string, string)))>;
     value: [12; "foo"; [1344; true]; (1, (2, ("foo", "bar")))]}
    |}]
;;

let%expect_test "validate an unit value" =
  let expr = Ast.unit () in
  let check = Validation.unit in
  expr |> check |> print dump_unit;
  [%expect {| null |}]
;;

let%expect_test "validate an non-unit value - 1" =
  let expr = Ast.null () in
  let check = Validation.unit in
  expr |> check |> print dump_unit;
  [%expect
    {|
    {message: "unexpected kind";
     expected: unit;
     given: null;
     value: <null>}
    |}]
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
  expr |> check |> print dump_unit;
  [%expect
    {|
    {message: "unexpected kind";
     expected: unit;
     given: list<int & string & list<int64 & bool> &
       (int, (int, (string, string)))>;
     value: [12; "foo"; [1344; true]; (1, (2, ("foo", "bar")))]}
    |}]
;;

let%expect_test "validate an unitish value - 1" =
  let expr = Ast.unit () in
  let check = Validation.unitish in
  expr |> check |> print dump_unit;
  [%expect {| null |}]
;;

let%expect_test "validate an unitish value - 2" =
  let expr = Ast.null () in
  let check = Validation.unitish in
  expr |> check |> print dump_unit;
  [%expect {| null |}]
;;

let%expect_test "validate a bool value - 1" =
  let expr = Ast.bool true in
  let check = Validation.bool in
  expr |> check |> print dump_bool;
  [%expect {| true |}]
;;

let%expect_test "validate a bool value - 2" =
  let expr = Ast.bool false in
  let check = Validation.bool in
  expr |> check |> print dump_bool;
  [%expect {| false |}]
;;

let%expect_test "validate a bool value in strict mode - 1" =
  let expr = Ast.bool true in
  let check = Validation.bool ~strict:true in
  expr |> check |> print dump_bool;
  [%expect {| true |}]
;;

let%expect_test "validate a bool value in strict mode - 2" =
  let expr = Ast.bool false in
  let check = Validation.bool ~strict:true in
  expr |> check |> print dump_bool;
  [%expect {| false |}]
;;

let%expect_test "validate a bool value in strict mode - 1" =
  let expr = Ast.bool true in
  let check = Validation.bool ~strict:true in
  expr |> check |> print dump_bool;
  [%expect {| true |}]
;;

let%expect_test "validate a bool value using string - 1" =
  let expr = Ast.string "TrUe" in
  let check = Validation.bool in
  expr |> check |> print dump_bool;
  [%expect {| true |}]
;;

let%expect_test "validate a bool value using string - 2" =
  let expr = Ast.string "  false " in
  let check = Validation.bool in
  expr |> check |> print dump_bool;
  [%expect {| false |}]
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
  expr |> check |> print dump_bool;
  [%expect
    {|
    {message: "unexpected kind";
     expected: bool;
     given: list<int & string & list<int64 & bool> &
       (int, (int, (string, string)))>;
     value: [12; "bar"; [1344; true]; (1, (2, ("foo", "bar")))]}
    |}]
;;

let%expect_test "validate a char value" =
  let expr = Ast.char 'x' in
  let check = Validation.char in
  expr |> check |> print dump_char;
  [%expect {| x |}]
;;

let%expect_test "validate a char value in strict mode" =
  let expr = Ast.char 'x' in
  let check = Validation.char ~strict:true in
  expr |> check |> print dump_char;
  [%expect {| x |}]
;;

let%expect_test "validate a char value using a string value" =
  let expr = Ast.string "x" in
  let check = Validation.char in
  expr |> check |> print dump_char;
  [%expect {| x |}]
;;

let%expect_test "validate a char value using an invalid string value" =
  let expr = Ast.string "xo" in
  let check = Validation.char in
  expr |> check |> print dump_char;
  [%expect
    {|
    {message: "unexpected kind";
     expected: char;
     given: string;
     value: "xo"}
    |}]
;;

let%expect_test "validate a char value using an invalid value" =
  let expr = Ast.int 45 in
  let check = Validation.char in
  expr |> check |> print dump_char;
  [%expect
    {|
    {message: "unexpected kind";
     expected: char;
     given: int;
     value: 45}
    |}]
;;

let%expect_test "validate a char value using a string value in strict mode" =
  let expr = Ast.string "x" in
  let check = Validation.char ~strict:true in
  expr |> check |> print dump_char;
  [%expect
    {|
    {message: "unexpected kind";
     expected: char;
     given: string;
     value: "x"}
    |}]
;;

let%expect_test "validate an int value" =
  let expr = Ast.int 42 in
  let check = Validation.int in
  expr |> check |> print dump_int;
  [%expect {| 42 |}]
;;

let%expect_test "validate an int value in strict mode" =
  let expr = Ast.int 42 in
  let check = Validation.int ~strict:true in
  expr |> check |> print dump_int;
  [%expect {| 42 |}]
;;

let%expect_test "validate an int value using a string" =
  let expr = Ast.string "42" in
  let check = Validation.int in
  expr |> check |> print dump_int;
  [%expect {| 42 |}]
;;

let%expect_test "validate an int value using an invalid string - 1" =
  let expr = Ast.string "foo" in
  let check = Validation.int in
  expr |> check |> print dump_int;
  [%expect
    {|
    {message: "unexpected kind";
     expected: int;
     given: string;
     value: "foo"}
    |}]
;;

let%expect_test "validate an int value using an invalid string - 2" =
  let expr = Ast.string "33sasa" in
  let check = Validation.int in
  expr |> check |> print dump_int;
  [%expect
    {|
    {message: "unexpected kind";
     expected: int;
     given: string;
     value: "33sasa"}
    |}]
;;

let%expect_test "validate an int value using a string in strict mode" =
  let expr = Ast.string "42" in
  let check = Validation.int ~strict:true in
  expr |> check |> print dump_int;
  [%expect
    {|
    {message: "unexpected kind";
     expected: int;
     given: string;
     value: "42"}
    |}]
;;

let%expect_test "validate an int value using an int32" =
  let expr = Ast.int32 42l in
  let check = Validation.int in
  expr |> check |> print dump_int;
  [%expect {| 42 |}]
;;

let%expect_test "validate an int value using an int32 in strict mode" =
  let expr = Ast.int32 42l in
  let check = Validation.int ~strict:true in
  expr |> check |> print dump_int;
  [%expect
    {|
    {message: "unexpected kind";
     expected: int;
     given: int32;
     value: 42}
    |}]
;;

let%expect_test "validate an int value using an int64" =
  let expr = Ast.int64 42L in
  let check = Validation.int in
  expr |> check |> print dump_int;
  [%expect {| 42 |}]
;;

let%expect_test "validate an int value using an invalid int64" =
  let expr = Ast.int64 Int64.max_int in
  let check = Validation.int in
  expr |> check |> print dump_int;
  [%expect
    {|
    {message: "unexpected kind";
     expected: int;
     given: int64;
     value: 9223372036854775807}
    |}]
;;

let%expect_test "validate an int value using an int64 in strict mode" =
  let expr = Ast.int64 42L in
  let check = Validation.int ~strict:true in
  expr |> check |> print dump_int;
  [%expect
    {|
    {message: "unexpected kind";
     expected: int;
     given: int64;
     value: 42}
    |}]
;;

let%expect_test "validate an int value using a float" =
  let expr = Ast.float 42. in
  let check = Validation.int in
  expr |> check |> print dump_int;
  [%expect {| 42 |}]
;;

let%expect_test "validate an int value using an invalid float" =
  let expr = Ast.float 42.32 in
  let check = Validation.int in
  expr |> check |> print dump_int;
  [%expect
    {|
    {message: "unexpected kind";
     expected: int;
     given: float;
     value: 42.32}
    |}]
;;

let%expect_test "validate an int value using a float in strict mode" =
  let expr = Ast.float 42. in
  let check = Validation.int ~strict:true in
  expr |> check |> print dump_int;
  [%expect
    {|
    {message: "unexpected kind";
     expected: int;
     given: float;
     value: 42}
    |}]
;;

let%expect_test "validate an int32 value" =
  let expr = Ast.int32 42l in
  let check = Validation.int32 in
  expr |> check |> print dump_int32;
  [%expect {| 42 |}]
;;

let%expect_test "validate an int32 value in strict mode" =
  let expr = Ast.int32 42l in
  let check = Validation.int32 ~strict:true in
  expr |> check |> print dump_int32;
  [%expect {| 42 |}]
;;

let%expect_test "validate an int32 value using an int" =
  let expr = Ast.int 42 in
  let check = Validation.int32 in
  expr |> check |> print dump_int32;
  [%expect {| 42 |}]
;;

let%expect_test "validate an int32 value using an int in strict mode" =
  let expr = Ast.int 42 in
  let check = Validation.int32 ~strict:true in
  expr |> check |> print dump_int32;
  [%expect
    {|
    {message: "unexpected kind";
     expected: int32;
     given: int;
     value: 42}
    |}]
;;

let%expect_test "validate an int32 value using an int64" =
  let expr = Ast.int64 42L in
  let check = Validation.int32 in
  expr |> check |> print dump_int32;
  [%expect {| 42 |}]
;;

let%expect_test "validate an int32 value using an invalid int64" =
  let expr = Ast.int64 Int64.max_int in
  let check = Validation.int32 in
  expr |> check |> print dump_int32;
  [%expect
    {|
    {message: "unexpected kind";
     expected: int32;
     given: int64;
     value: 9223372036854775807}
    |}]
;;

let%expect_test "validate an int32 value using an int64 in strict mode" =
  let expr = Ast.int64 42L in
  let check = Validation.int32 ~strict:true in
  expr |> check |> print dump_int32;
  [%expect
    {|
    {message: "unexpected kind";
     expected: int32;
     given: int64;
     value: 42}
    |}]
;;

let%expect_test "validate an int32 value using a float" =
  let expr = Ast.float 42. in
  let check = Validation.int32 in
  expr |> check |> print dump_int32;
  [%expect {| 42 |}]
;;

let%expect_test "validate an int32 value using a float in strict mode" =
  let expr = Ast.float 42. in
  let check = Validation.int32 ~strict:true in
  expr |> check |> print dump_int32;
  [%expect
    {|
    {message: "unexpected kind";
     expected: int32;
     given: float;
     value: 42}
    |}]
;;

let%expect_test "validate an int32 value using an invalid float" =
  let expr = Ast.float 3.14 in
  let check = Validation.int32 in
  expr |> check |> print dump_int32;
  [%expect
    {|
    {message: "unexpected kind";
     expected: int32;
     given: float;
     value: 3.14}
    |}]
;;

let%expect_test "validate an int32 value using a string" =
  let expr = Ast.string "42" in
  let check = Validation.int32 in
  expr |> check |> print dump_int32;
  [%expect {| 42 |}]
;;

let%expect_test "validate an int32 value using a string in strict mode" =
  let expr = Ast.string "42" in
  let check = Validation.int32 ~strict:true in
  expr |> check |> print dump_int32;
  [%expect
    {|
    {message: "unexpected kind";
     expected: int32;
     given: string;
     value: "42"}
    |}]
;;

let%expect_test "validate an int32 value using an invalid string" =
  let expr = Ast.string "hello" in
  let check = Validation.int32 in
  expr |> check |> print dump_int32;
  [%expect
    {|
    {message: "unexpected kind";
     expected: int32;
     given: string;
     value: "hello"}
    |}]
;;

let%expect_test "validate an int64 value" =
  let expr = Ast.int64 42L in
  let check = Validation.int64 in
  expr |> check |> print dump_int64;
  [%expect {| 42 |}]
;;

let%expect_test "validate an int64 value in strict mode" =
  let expr = Ast.int64 42L in
  let check = Validation.int64 ~strict:true in
  expr |> check |> print dump_int64;
  [%expect {| 42 |}]
;;

let%expect_test "validate an int value as an int64" =
  let expr = Ast.int 42 in
  let check = Validation.int64 in
  expr |> check |> print dump_int64;
  [%expect {| 42 |}]
;;

let%expect_test "validate an int value as an int64 in strict mode" =
  let expr = Ast.int 42 in
  let check = Validation.int64 ~strict:true in
  expr |> check |> print dump_int64;
  [%expect
    {|
    {message: "unexpected kind";
     expected: int64;
     given: int;
     value: 42}
    |}]
;;

let%expect_test "validate an int32 value as an int64" =
  let expr = Ast.int32 42l in
  let check = Validation.int64 in
  expr |> check |> print dump_int64;
  [%expect {| 42 |}]
;;

let%expect_test "validate an int32 value as an int64 in strict mode" =
  let expr = Ast.int32 42l in
  let check = Validation.int64 ~strict:true in
  expr |> check |> print dump_int64;
  [%expect
    {|
    {message: "unexpected kind";
     expected: int64;
     given: int32;
     value: 42}
    |}]
;;

let%expect_test "validate a float value as an int64" =
  let expr = Ast.float 42.0 in
  let check = Validation.int64 in
  expr |> check |> print dump_int64;
  [%expect {| 42 |}]
;;

let%expect_test "validate an invalid float value as an int64" =
  let expr = Ast.float 42.22 in
  let check = Validation.int64 in
  expr |> check |> print dump_int64;
  [%expect
    {|
    {message: "unexpected kind";
     expected: int64;
     given: float;
     value: 42.22}
    |}]
;;

let%expect_test "validate a float value as an int64 in strict mode" =
  let expr = Ast.float 42.0 in
  let check = Validation.int64 ~strict:true in
  expr |> check |> print dump_int64;
  [%expect
    {|
    {message: "unexpected kind";
     expected: int64;
     given: float;
     value: 42}
    |}]
;;

let%expect_test "validate a float value as an int64" =
  let expr = Ast.float 42.0 in
  let check = Validation.int64 in
  expr |> check |> print dump_int64;
  [%expect {| 42 |}]
;;

let%expect_test "validate a string value as an int64" =
  let expr = Ast.string "345678909" in
  let check = Validation.int64 in
  expr |> check |> print dump_int64;
  [%expect {| 345678909 |}]
;;

let%expect_test "validate a string value as an invalid int64" =
  let expr = Ast.string "kkk345678909" in
  let check = Validation.int64 in
  expr |> check |> print dump_int64;
  [%expect
    {|
    {message: "unexpected kind";
     expected: int64;
     given: string;
     value: "kkk345678909"}
    |}]
;;

let%expect_test "validate a string value as an int64 in strict mode" =
  let expr = Ast.string "345678909" in
  let check = Validation.int64 ~strict:true in
  expr |> check |> print dump_int64;
  [%expect
    {|
    {message: "unexpected kind";
     expected: int64;
     given: string;
     value: "345678909"}
    |}]
;;

let%expect_test "validate a float" =
  let expr = Ast.float 42.0 in
  let check = Validation.float in
  expr |> check |> print dump_float;
  [%expect {| 42 |}]
;;

let%expect_test "validate a float in strict mode" =
  let expr = Ast.float 42.0 in
  let check = Validation.float ~strict:true in
  expr |> check |> print dump_float;
  [%expect {| 42 |}]
;;

let%expect_test "validate a float using an int" =
  let expr = Ast.int 42 in
  let check = Validation.float in
  expr |> check |> print dump_float;
  [%expect {| 42 |}]
;;

let%expect_test "validate a float using an int in strict mode" =
  let expr = Ast.int 42 in
  let check = Validation.float ~strict:true in
  expr |> check |> print dump_float;
  [%expect
    {|
    {message: "unexpected kind";
     expected: float;
     given: int;
     value: 42}
    |}]
;;

let%expect_test "validate a float using an int32" =
  let expr = Ast.int32 42l in
  let check = Validation.float in
  expr |> check |> print dump_float;
  [%expect {| 42 |}]
;;

let%expect_test "validate a float using an int32 in strict mode" =
  let expr = Ast.int32 42l in
  let check = Validation.float ~strict:true in
  expr |> check |> print dump_float;
  [%expect
    {|
    {message: "unexpected kind";
     expected: float;
     given: int32;
     value: 42}
    |}]
;;

let%expect_test "validate a float using an int64" =
  let expr = Ast.int64 42L in
  let check = Validation.float in
  expr |> check |> print dump_float;
  [%expect {| 42 |}]
;;

let%expect_test "validate a float using an int64 in strict mode" =
  let expr = Ast.int64 42L in
  let check = Validation.float ~strict:true in
  expr |> check |> print dump_float;
  [%expect
    {|
    {message: "unexpected kind";
     expected: float;
     given: int64;
     value: 42}
    |}]
;;

let%expect_test "validate a float using a string - 1" =
  let expr = Ast.string "3.14" in
  let check = Validation.float in
  expr |> check |> print dump_float;
  [%expect {| 3.14 |}]
;;

let%expect_test "validate a float using a string - 2" =
  let expr = Ast.string "3" in
  let check = Validation.float in
  expr |> check |> print dump_float;
  [%expect {| 3 |}]
;;

let%expect_test "validate a float using an invalid string" =
  let expr = Ast.string "foo" in
  let check = Validation.float in
  expr |> check |> print dump_float;
  [%expect
    {|
    {message: "unexpected kind";
     expected: float;
     given: string;
     value: "foo"}
    |}]
;;

let%expect_test "validate a float using a string in strict mode" =
  let expr = Ast.string "3.14" in
  let check = Validation.float ~strict:true in
  expr |> check |> print dump_float;
  [%expect
    {|
    {message: "unexpected kind";
     expected: float;
     given: string;
     value: "3.14"}
    |}]
;;

let%expect_test "validate an integer - 1" =
  let expr = Ast.int 1 in
  let check = Validation.integer ~strict:true in
  expr |> check |> print dump_int64;
  [%expect {| 1 |}]
;;

let%expect_test "validate an integer - 2" =
  let expr = Ast.int32 1l in
  let check = Validation.integer ~strict:true in
  expr |> check |> print dump_int64;
  [%expect {| 1 |}]
;;

let%expect_test "validate an integer - 3" =
  let expr = Ast.int64 2L in
  let check = Validation.integer ~strict:true in
  expr |> check |> print dump_int64;
  [%expect {| 2 |}]
;;

let%expect_test "validate an integer with a float" =
  let expr = Ast.float 2.33 in
  let check = Validation.integer ~strict:true in
  expr |> check |> print dump_int64;
  [%expect
    {|
    {message: "unexpected kind";
     expected: int | int32 | int64;
     given: float;
     value: 2.33}
    |}]
;;

let%expect_test "validate an integer with a float in non-strict mode" =
  let expr = Ast.float 2.0 in
  let check = Validation.integer in
  expr |> check |> print dump_int64;
  [%expect {| 2 |}]
;;

let%expect_test "validate a number - 1" =
  let expr = Ast.int 1 in
  let check = Validation.number ~strict:true in
  expr |> check |> print dump_float;
  [%expect {| 1 |}]
;;

let%expect_test "validate a number - 2" =
  let expr = Ast.int32 1l in
  let check = Validation.number ~strict:true in
  expr |> check |> print dump_float;
  [%expect {| 1 |}]
;;

let%expect_test "validate a number - 3" =
  let expr = Ast.int64 1L in
  let check = Validation.number ~strict:true in
  expr |> check |> print dump_float;
  [%expect {| 1 |}]
;;

let%expect_test "validate a number - 4" =
  let expr = Ast.float 3.14 in
  let check = Validation.number ~strict:true in
  expr |> check |> print dump_float;
  [%expect {| 3.14 |}]
;;

let%expect_test "validate an invalid number" =
  let expr = Ast.string "Foo" in
  let check = Validation.number in
  expr |> check |> print dump_float;
  [%expect
    {|
    {message: "unexpected kind";
     expected: int | int32 | int64 | float;
     given: string;
     value: "Foo"}
    |}]
;;

let%expect_test "validate a string - 1" =
  let expr = Ast.string "Hello World" in
  let check = Validation.string in
  expr |> check |> print dump_string;
  [%expect {| "Hello World" |}]
;;

let%expect_test "validate a string - 2" =
  let expr = Ast.bool true in
  let check = Validation.string in
  expr |> check |> print dump_string;
  [%expect {| "true" |}]
;;

let%expect_test "validate a string - 3" =
  let expr = Ast.bool false in
  let check = Validation.string in
  expr |> check |> print dump_string;
  [%expect {| "false" |}]
;;

let%expect_test "validate a string in strict mode" =
  let expr = Ast.string "Hello World" in
  let check = Validation.string ~strict:true in
  expr |> check |> print dump_string;
  [%expect {| "Hello World" |}]
;;

let%expect_test "validate something else in strict mode" =
  let expr = Ast.float 32.0 in
  let check = Validation.string ~strict:true in
  expr |> check |> print dump_string;
  [%expect
    {|
    {message: "unexpected kind";
     expected: string;
     given: float;
     value: 32}
    |}]
;;

let%expect_test "validate a pair - 1" =
  let expr = Ast.(pair' unit unit () ()) in
  let check = Validation.(pair unit unit) in
  expr |> check |> print @@ dump_pair dump_unit dump_unit;
  [%expect {| (null, null) |}]
;;

let%expect_test "validate a pair - 1" =
  let expr = Ast.(pair' int string 42 "foo") in
  let check = Validation.(pair int string) in
  expr |> check |> print @@ dump_pair dump_int dump_string;
  [%expect {| (42, "foo") |}]
;;

let%expect_test "validate a pair using an invalid shape - 1" =
  let expr = Ast.string "foo" in
  let check = Validation.(pair int string) in
  expr |> check |> print @@ dump_pair dump_int dump_string;
  [%expect
    {|
    {message: "unexpected kind";
     expected: (?any, ?any);
     given: string;
     value: "foo"}
    |}]
;;

let%expect_test "validate a pair using an invalid shape - 2" =
  let expr = Ast.(pair' string int "foo" 42) in
  let check = Validation.(pair int string) in
  expr |> check |> print @@ dump_pair dump_int dump_string;
  [%expect
    {|
    {message: "unexpected pair";
     where:
      {on: "first";
       error:
        {message: "unexpected kind";
         expected: int;
         given: string;
         value: "foo"}};
     given: (string, int);
     value: ("foo", 42)}
    |}]
;;

let%expect_test "validate a pair using an invalid shape - 3" =
  let expr = Ast.(pair' string int "foo" 42) in
  let check = Validation.(pair (int ~strict:true) (string ~strict:true)) in
  expr |> check |> print @@ dump_pair dump_int dump_string;
  [%expect
    {|
    {message: "unexpected pair";
     where:
      {on: "both";
       first_error:
        {message: "unexpected kind";
         expected: int;
         given: string;
         value: "foo"};
       second_error:
        {message: "unexpected kind";
         expected: string;
         given: int;
         value: 42}};
     given: (string, int);
     value: ("foo", 42)}
    |}]
;;

let%expect_test "validate a pair using an invalid shape - 4" =
  let expr = Ast.(pair' int int 12 42) in
  let check = Validation.(pair (int ~strict:true) (string ~strict:true)) in
  expr |> check |> print @@ dump_pair dump_int dump_string;
  [%expect
    {|
    {message: "unexpected pair";
     where:
      {on: "second";
       error: {message: "unexpected kind";
               expected: string;
               given: int;
               value: 42}};
     given: (int, int);
     value: (12, 42)}
    |}]
;;

let%expect_test "validate a pair using an invalid shape - 5" =
  let expr = Ast.(pair' string string "foo" "42") in
  let check = Validation.(pair (int ~strict:true) (string ~strict:true)) in
  expr |> check |> print @@ dump_pair dump_int dump_string;
  [%expect
    {|
    {message: "unexpected pair";
     where:
      {on: "first";
       error:
        {message: "unexpected kind";
         expected: int;
         given: string;
         value: "foo"}};
     given: (string, string);
     value: ("foo", "42")}
    |}]
;;

let%expect_test "validate a quad - 1" =
  let expr = Ast.(quad' int int int int 1 2 3 4) in
  let check = Validation.(quad int int int int) in
  expr |> check |> print @@ dump_quad dump_int dump_int dump_int dump_int;
  [%expect {| (1, (2, (3, 4))) |}]
;;

let%expect_test "validate a quad using a list - 2" =
  let expr = Ast.(hlist [ int 1; bool true; string "Hello"; float 32.21 ]) in
  let check = Validation.(quad int bool string float) in
  expr |> check |> print @@ dump_quad dump_int dump_bool dump_string dump_float;
  [%expect {| (1, (true, ("Hello", 32.21))) |}]
;;

let%expect_test "validate an invalid quad - 1" =
  let expr = Ast.(quad' null int bool int 1 2 true 4) in
  let check = Validation.(quad int int int int) in
  expr |> check |> print @@ dump_quad dump_int dump_int dump_int dump_int;
  [%expect
    {|
    {message: "unexpected pair";
     where:
      {on: "both";
       first_error:
        {message: "unexpected kind";
         expected: int;
         given: null;
         value: <null>};
       second_error:
        {message: "unexpected pair";
         where:
          {on: "second";
           error:
            {message: "unexpected pair";
             where:
              {on: "first";
               error:
                {message: "unexpected kind";
                 expected: int;
                 given: bool;
                 value: true}};
             given: (bool, int);
             value: (true, 4)}};
         given: (int, (bool, int));
         value: (2, (true, 4))}};
     given: (null, (int, (bool, int)));
     value: (<null>, (2, (true, 4)))}
    |}]
;;

let%expect_test "validate an invalid quad using a list - 2" =
  let expr = Ast.(hlist [ int 1; bool true; string "Hello"; float 32.21 ]) in
  let check = Validation.(quad int int int int) in
  expr |> check |> print @@ dump_quad dump_int dump_int dump_int dump_int;
  [%expect
    {|
    {message: "unexpected pair";
     where:
      {on: "second";
       error:
        {message: "unexpected pair";
         where:
          {on: "both";
           first_error:
            {message: "unexpected kind";
             expected: int;
             given: bool;
             value: true};
           second_error:
            {message: "unexpected pair";
             where:
              {on: "both";
               first_error:
                {message: "unexpected kind";
                 expected: int;
                 given: string;
                 value: "Hello"};
               second_error:
                {message: "unexpected kind";
                 expected: int;
                 given: float;
                 value: 32.21}};
             given: (string, float);
             value: ("Hello", 32.21)}};
         given: (bool, (string, float));
         value: (true, ("Hello", 32.21))}};
     given: (int, (bool, (string, float)));
     value: (1, (true, ("Hello", 32.21)))}
    |}]
;;
