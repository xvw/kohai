let dump = function
  | Some rensai_expr ->
    rensai_expr |> Format.asprintf "Some %a" Rensai.Ast.pp |> print_endline
  | None -> print_endline "None"
;;

let dump_list res =
  res |> Format.asprintf "%a" (Fmt.Dump.list Rensai.Lang.pp) |> print_endline
;;

let%expect_test "Lang.from_string - 1" =
  let input = {rs||rs} in
  input |> Rensai.Lang.from_string |> dump;
  [%expect {| None |}]
;;

let%expect_test "Lang.from_string - 2" =
  let input = {rs|()|rs} in
  input |> Rensai.Lang.from_string |> dump;
  [%expect {| Some <unit> |}]
;;

let%expect_test "Lang.from_string - 3" =
  let input = {rs|123|rs} in
  input |> Rensai.Lang.from_string |> dump;
  [%expect {| Some 123 |}]
;;

let%expect_test "Lang.from_string - 4" =
  let input = {rs|123.65|rs} in
  input |> Rensai.Lang.from_string |> dump;
  [%expect {| Some 123.65 |}]
;;

let%expect_test "Lang.from_string - 5" =
  let input = {rs|["foo", 'f']|rs} in
  input |> Rensai.Lang.from_string |> dump;
  [%expect {| Some ["foo"; 'f'] |}]
;;

let%expect_test "Lang.from_string - 5" =
  let input =
    {rs|<
     id: "foo";
     length: 42;
     isActive: true;
     gender: #Male(());
     lList: #Cons((true, #Nil(())));
     sub: <hd: [1, 2, 3]; tl:(true, (1, false))>>|rs}
  in
  input |> Rensai.Lang.from_string |> dump;
  [%expect
    {|
    Some {gender = male(<unit>); id = "foo"; isActive = true;
           lList = cons((true, nil(<unit>))); length = 42;
           sub = {hd = [1; 2; 3]; tl = (true, (1, false))}}
    |}]
;;

let%expect_test "Read multiple expressionm from lexing buf - 1" =
  let input =
    {rs|
     <foo: 1; bar: true>
     <x: #foo(1); desc:"foo bar baz">
  |rs}
  in
  let lexbuf = Lexing.from_string input in
  let result = Rensai.Lang.from_lexingbuf_to_list ~reverse:true lexbuf in
  result |> dump_list;
  [%expect {| [<bar: true; foo: 1>; <desc: "foo bar baz"; x: #foo(1)>] |}]
;;
