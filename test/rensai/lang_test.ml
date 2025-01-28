let dump = function
  | Some rensai_expr ->
    rensai_expr |> Format.asprintf "Some %a" Rensai.Ast.pp |> print_endline
  | None -> print_endline "None"
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
  let input =
    {rs|<
     id: "foo";
     length: 42;
     isActive: true;
     sub: <hd: [1, 2, 3]; tl:(true, (1, false))>>|rs}
  in
  input |> Rensai.Lang.from_string |> dump;
  [%expect {|
    Some {id = "foo"; isActive = true; length = 42;
           sub = {hd = [1; 2; 3]; tl = (true, (1, false))}}
    |}]
;;
