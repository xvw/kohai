open Prelude

let dump pp value =
  value |> Format.asprintf "nel@[<h>[%a]@]" (Nel.pp pp) |> print_endline
;;

let%expect_test "make - 1" =
  let expr = Nel.make 1 [] in
  expr |> dump Format.pp_print_int;
  [%expect {| nel[1] |}]
;;

let%expect_test "make - 2" =
  let expr = Nel.make 1 [ 2; 3; 4 ] in
  expr |> dump Format.pp_print_int;
  [%expect {| nel[1; 2; 3; 4] |}]
;;

let%expect_test "equal, make, singleton - 1" =
  let nel_a = Nel.make 1 [] in
  let nel_b = Nel.singleton 1 in
  if Nel.equal Int.equal nel_a nel_b
  then print_endline "ok"
  else print_endline "not-ok";
  [%expect {| ok |}]
;;

let%expect_test "cons - 1" =
  let expr = Nel.(cons 1 @@ singleton 2) in
  expr |> dump Format.pp_print_int;
  [%expect {| nel[1; 2] |}]
;;

let%expect_test "rev - 1" =
  let expr = Nel.(1 :: 2 :: 3 :: [ 4 ] |> rev) in
  expr |> dump Format.pp_print_int;
  [%expect {| nel[4; 3; 2; 1] |}]
;;

let%expect_test "append - 1" =
  let expr = Nel.(1 :: 2 :: 3 :: [ 4 ] |> append (0 :: [])) in
  expr |> dump Format.pp_print_int;
  [%expect {| nel[0; 1; 2; 3; 4] |}]
;;

let%expect_test "append - 2" =
  let expr = Nel.(1 :: 2 :: 3 :: [ 4 ] |> append (-2 :: -1 :: [ 0 ])) in
  expr |> dump Format.pp_print_int;
  [%expect {| nel[-2; -1; 0; 1; 2; 3; 4] |}]
;;
