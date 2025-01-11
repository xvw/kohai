open Rensai

let print pp value =
  value |> Fmt.str "%a" (Validation.pp_checked pp) |> print_endline
;;

let%expect_test "where - 1" =
  let subject = Ast.int 180
  and checker = Validation.(int & where ~pp:Fmt.int (fun x -> x + 1 = 181)) in
  subject |> checker |> print Fmt.int;
  [%expect {| 180 |}]
;;

let%expect_test "where - 2" =
  let subject = Ast.int 170
  and checker = Validation.(int & where ~pp:Fmt.int (fun x -> x + 1 = 181)) in
  subject |> checker |> print Fmt.int;
  [%expect
    {|
    {message: "unexpected value";
     error: "unsatisfied predicate on 170"}
    |}]
;;

let%expect_test "unless - 1" =
  let subject = Ast.int 180
  and checker = Validation.(int & unless ~pp:Fmt.int (fun x -> x + 1 = 181)) in
  subject |> checker |> print Fmt.int;
  [%expect
    {|
    {message: "unexpected value";
     error: "satisfied predicate on 180"}
    |}]
;;

let%expect_test "unless - 2" =
  let subject = Ast.int 170
  and checker = Validation.(int & unless ~pp:Fmt.int (fun x -> x + 1 = 181)) in
  subject |> checker |> print Fmt.int;
  [%expect {| 170 |}]
;;

let%expect_test "equal - 1" =
  let subject = Ast.int 180
  and checker = Validation.(int & equal ~pp:Fmt.int 180) in
  subject |> checker |> print Fmt.int;
  [%expect {| 180 |}]
;;

let%expect_test "equal - 2" =
  let subject = Ast.int 180
  and checker = Validation.(int & equal ~pp:Fmt.int 182) in
  subject |> checker |> print Fmt.int;
  [%expect
    {|
    {message: "unexpected value";
     error: "`a` (182) is not equal to `b` (180)"}
    |}]
;;

let%expect_test "refute - 1" =
  let subject = Ast.int 182
  and checker = Validation.(int & refute ~pp:Fmt.int (equal 182)) in
  subject |> checker |> print Fmt.int;
  [%expect
    {|
    {message: "unexpected value";
     error: "refuted validator on 182"}
    |}]
;;

let%expect_test "refute - 2" =
  let subject = Ast.int 180
  and checker = Validation.(int & refute ~pp:Fmt.int (equal 182)) in
  subject |> checker |> print Fmt.int;
  [%expect {| 180 |}]
;;

let%expect_test "not_equal - 1" =
  let subject = Ast.int 180
  and checker = Validation.(int & not_equal ~pp:Fmt.int 182) in
  subject |> checker |> print Fmt.int;
  [%expect {| 180 |}]
;;

let%expect_test "not_equal - 2" =
  let subject = Ast.int 180
  and checker = Validation.(int & not_equal ~pp:Fmt.int 180) in
  subject |> checker |> print Fmt.int;
  [%expect
    {|
    {message: "unexpected value";
     error: "`a` (180) is equal to `b` (180)"}
    |}]
;;

let%expect_test "greater - 1" =
  let subject = Ast.int 180
  and checker = Validation.(int & greater ~pp:Fmt.int ~than:170) in
  subject |> checker |> print Fmt.int;
  [%expect {| 180 |}]
;;

let%expect_test "greater - 2" =
  let subject = Ast.int 180
  and checker = Validation.(int & greater ~pp:Fmt.int ~than:180) in
  subject |> checker |> print Fmt.int;
  [%expect
    {|
    {message: "unexpected value";
     error: "`a` (180) is not greater than `b` (180)"}
    |}]
;;

let%expect_test "greater - 3" =
  let subject = Ast.int 180
  and checker = Validation.(int & greater ~pp:Fmt.int ~than:190) in
  subject |> checker |> print Fmt.int;
  [%expect
    {|
    {message: "unexpected value";
     error: "`a` (190) is not greater than `b` (180)"}
    |}]
;;

let%expect_test "greater_or_equal - 1" =
  let subject = Ast.int 180
  and checker = Validation.(int & greater_or_equal ~pp:Fmt.int ~than:170) in
  subject |> checker |> print Fmt.int;
  [%expect {| 180 |}]
;;

let%expect_test "greater_or_equal - 2" =
  let subject = Ast.int 180
  and checker = Validation.(int & greater_or_equal ~pp:Fmt.int ~than:180) in
  subject |> checker |> print Fmt.int;
  [%expect {| 180 |}]
;;

let%expect_test "greater_or_equal - 3" =
  let subject = Ast.int 180
  and checker = Validation.(int & greater_or_equal ~pp:Fmt.int ~than:190) in
  subject |> checker |> print Fmt.int;
  [%expect
    {|
    {message: "unexpected value";
     error: "`a` (190) is not greater or equal to `b` (180)"}
    |}]
;;

let%expect_test "less - 1" =
  let subject = Ast.int 180
  and checker = Validation.(int & less ~pp:Fmt.int ~than:190) in
  subject |> checker |> print Fmt.int;
  [%expect {| 180 |}]
;;

let%expect_test "less - 2" =
  let subject = Ast.int 180
  and checker = Validation.(int & less ~pp:Fmt.int ~than:180) in
  subject |> checker |> print Fmt.int;
  [%expect
    {|
    {message: "unexpected value";
     error: "`a` (180) is not less than `b` (180)"}
    |}]
;;

let%expect_test "less - 3" =
  let subject = Ast.int 180
  and checker = Validation.(int & less ~pp:Fmt.int ~than:170) in
  subject |> checker |> print Fmt.int;
  [%expect
    {|
    {message: "unexpected value";
     error: "`a` (170) is not less than `b` (180)"}
    |}]
;;

let%expect_test "less_or_equal - 1" =
  let subject = Ast.int 180
  and checker = Validation.(int & less_or_equal ~pp:Fmt.int ~than:190) in
  subject |> checker |> print Fmt.int;
  [%expect {| 180 |}]
;;

let%expect_test "less_or_equal - 2" =
  let subject = Ast.int 180
  and checker = Validation.(int & less_or_equal ~pp:Fmt.int ~than:180) in
  subject |> checker |> print Fmt.int;
  [%expect {| 180 |}]
;;

let%expect_test "less_or_equal - 3" =
  let subject = Ast.int 180
  and checker = Validation.(int & less_or_equal ~pp:Fmt.int ~than:170) in
  subject |> checker |> print Fmt.int;
  [%expect
    {|
    {message: "unexpected value";
     error: "`a` (170) is not less or equal to `b` (180)"}
    |}]
;;

let%expect_test "in_range - 1" =
  let subject = Ast.int 180
  and checker = Validation.(int & in_range ~pp:Fmt.int ~min:170 ~max:190) in
  subject |> checker |> print Fmt.int;
  [%expect {| 180 |}]
;;

let%expect_test "in_range - 2" =
  let subject = Ast.int 160
  and checker = Validation.(int & in_range ~pp:Fmt.int ~min:170 ~max:190) in
  subject |> checker |> print Fmt.int;
  [%expect
    {|
    {message: "unexpected value";
     error: "`a` (160) is not included into [`min` (170); `max` (190)]"}
    |}]
;;

let%expect_test "in_range - 3" =
  let subject = Ast.int 200
  and checker = Validation.(int & in_range ~pp:Fmt.int ~min:170 ~max:190) in
  subject |> checker |> print Fmt.int;
  [%expect
    {|
    {message: "unexpected value";
     error: "`a` (200) is not included into [`min` (170); `max` (190)]"}
    |}]
;;

let%expect_test "in_range - 4" =
  let subject = Ast.int 170
  and checker = Validation.(int & in_range ~pp:Fmt.int ~min:170 ~max:190) in
  subject |> checker |> print Fmt.int;
  [%expect {| 170 |}]
;;

let%expect_test "in_range - 5" =
  let subject = Ast.int 190
  and checker = Validation.(int & in_range ~pp:Fmt.int ~min:170 ~max:190) in
  subject |> checker |> print Fmt.int;
  [%expect {| 190 |}]
;;

let%expect_test "outside_range - 1" =
  let subject = Ast.int 180
  and checker =
    Validation.(int & outside_range ~pp:Fmt.int ~min:170 ~max:190)
  in
  subject |> checker |> print Fmt.int;
  [%expect
    {|
    {message: "unexpected value";
     error: "`a` (180) is included into [`min` (170); `max` (190)]"}
    |}]
;;

let%expect_test "outside_range - 2" =
  let subject = Ast.int 160
  and checker =
    Validation.(int & outside_range ~pp:Fmt.int ~min:170 ~max:190)
  in
  subject |> checker |> print Fmt.int;
  [%expect {| 160 |}]
;;

let%expect_test "outside_range - 3" =
  let subject = Ast.int 200
  and checker =
    Validation.(int & outside_range ~pp:Fmt.int ~min:170 ~max:190)
  in
  subject |> checker |> print Fmt.int;
  [%expect {| 200 |}]
;;

let%expect_test "outside_range - 4" =
  let subject = Ast.int 170
  and checker =
    Validation.(int & outside_range ~pp:Fmt.int ~min:170 ~max:190)
  in
  subject |> checker |> print Fmt.int;
  [%expect
    {|
    {message: "unexpected value";
     error: "`a` (170) is included into [`min` (170); `max` (190)]"}
    |}]
;;

let%expect_test "outside_range - 5" =
  let subject = Ast.int 190
  and checker =
    Validation.(int & outside_range ~pp:Fmt.int ~min:170 ~max:190)
  in
  subject |> checker |> print Fmt.int;
  [%expect
    {|
    {message: "unexpected value";
     error: "`a` (190) is included into [`min` (170); `max` (190)]"}
    |}]
;;

let%expect_test "one_of - 1" =
  let subject = Ast.string "foo"
  and checker =
    Validation.(string ~strict:true & one_of ~pp:Fmt.string [ "foo"; "bar" ])
  in
  subject |> checker |> print Fmt.string;
  [%expect {| foo |}]
;;

let%expect_test "one_of - 2" =
  let subject = Ast.string "baz"
  and checker =
    Validation.(string ~strict:true & one_of ~pp:Fmt.string [ "foo"; "bar" ])
  in
  subject |> checker |> print Fmt.string;
  [%expect
    {|
    {message: "unexpected value";
     error: "baz is not in the list [foo; bar]"}
    |}]
;;

let%expect_test "ensure that pp are properly reported in specific validator - 1"
  =
  let subject = Ast.int 180
  and checker = Validation.(int & Int.greater ~than:160) in
  subject |> checker |> print Fmt.int;
  [%expect {| 180 |}]
;;

let%expect_test "ensure that pp are properly reported in specific validator - 2"
  =
  let subject = Ast.int 180
  and checker = Validation.(int & Int.greater ~than:190) in
  subject |> checker |> print Fmt.int;
  [%expect
    {|
    {message: "unexpected value";
     error: "`a` (190) is not greater than `b` (180)"}
    |}]
;;

let%expect_test "char is_digit - 1" =
  let subject = Ast.char '1'
  and checker = Validation.(char & Char.is_digit) in
  subject |> checker |> print Fmt.char;
  [%expect {| 1 |}]
;;

let%expect_test "char is_digit - 2" =
  let subject = Ast.char 'a'
  and checker = Validation.(char & Char.is_digit) in
  subject |> checker |> print Fmt.char;
  [%expect
    {|
    {message: "unexpected value";
     error: "`a` is not a '0' .. '9'"}
    |}]
;;

let%expect_test "char as_digit - 1" =
  let subject = Ast.(list char) [ '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9' ]
  and checker = Validation.(list_of (char & Char.is_digit)) in
  subject |> checker |> print (Fmt.Dump.list Fmt.char);
  [%expect {| [1; 2; 3; 4; 5; 6; 7; 8; 9] |}]
;;

let%expect_test "char as_digit - 2" =
  let subject =
    Ast.(list char)
      [ '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9'; 'a'; 'B'; 'f'; 'z' ]
  and checker = Validation.(list_of (char & Char.is_digit)) in
  subject |> checker |> print (Fmt.Dump.list Fmt.char);
  [%expect
    {|
    {message: "unexpected list";
     where:
      [09:{message: "unexpected value";
           error: "`a` is not a '0' .. '9'"};
       10:{message: "unexpected value";
           error: "`B` is not a '0' .. '9'"};
       11:{message: "unexpected value";
           error: "`f` is not a '0' .. '9'"};
       12:{message: "unexpected value";
           error: "`z` is not a '0' .. '9'"}];
     given: list<char>;
     value: ['1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9'; 'a'; 'B'; 'f'; 'z']}
    |}]
;;

let%expect_test "char is_hex digit - 1" =
  let subject = Ast.char 'F'
  and checker = Validation.(char & Char.is_hex_digit) in
  subject |> checker |> print Fmt.char;
  [%expect {| F |}]
;;

let%expect_test "char is_hex digit - 2" =
  let subject = Ast.char 'z'
  and checker = Validation.(char & Char.is_hex_digit) in
  subject |> checker |> print Fmt.char;
  [%expect
    {|
    {message: "unexpected value";
     error: "`z` is not a '0' .. '9' or 'a' .. 'f'"}
    |}]
;;

let%expect_test "char as_hex_digit - 1" =
  let subject =
    Ast.(list char)
      [ '1'
      ; '2'
      ; '3'
      ; '4'
      ; '5'
      ; '6'
      ; '7'
      ; '8'
      ; '9'
      ; 'a'
      ; 'b'
      ; 'c'
      ; 'd'
      ; 'e'
      ; 'f'
      ; 'A'
      ; 'B'
      ; 'C'
      ; 'D'
      ; 'E'
      ; 'F'
      ]
  and checker = Validation.(list_of (char & Char.as_hex_digit)) in
  subject |> checker |> print (Fmt.Dump.list Fmt.int);
  [%expect
    {| [1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15; 10; 11; 12; 13; 14; 15] |}]
;;

let%expect_test "char as_hex_digit - 2" =
  let subject =
    Ast.(list char)
      [ '1'
      ; '2'
      ; '3'
      ; '4'
      ; '5'
      ; '6'
      ; '7'
      ; '8'
      ; '9'
      ; 'a'
      ; 'b'
      ; 'c'
      ; 'd'
      ; 'e'
      ; 'f'
      ; 'A'
      ; 'B'
      ; 'C'
      ; 'D'
      ; 'E'
      ; 'F'
      ; 'g'
      ; 'G'
      ; 'H'
      ]
  and checker = Validation.(list_of (char & Char.as_hex_digit)) in
  subject |> checker |> print (Fmt.Dump.list Fmt.int);
  [%expect
    {|
    {message: "unexpected list";
     where:
      [21:
       {message: "unexpected value";
        error: "`g` is not a '0' .. '9' or 'a' .. 'f'"};
       22:
       {message: "unexpected value";
        error: "`G` is not a '0' .. '9' or 'a' .. 'f'"};
       23:
       {message: "unexpected value";
        error: "`H` is not a '0' .. '9' or 'a' .. 'f'"}];
     given: list<char>;
     value:
      ['1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9'; 'a'; 'b'; 'c'; 'd'; 'e';
       'f'; 'A'; 'B'; 'C'; 'D'; 'E'; 'F'; 'g'; 'G'; 'H']}
    |}]
;;
