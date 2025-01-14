let as_int64 x = Scanf.sscanf_opt x "%LdL%!" (fun x -> Int64.to_string x)
let as_int32 x = Scanf.sscanf_opt x "%ldl%!" (fun x -> Int32.to_string x)
let as_int x = Scanf.sscanf_opt x "%d%!" (fun x -> Int.to_string x)

let print = function
  | None -> print_endline "none"
  | Some x -> print_endline @@ "some " ^ x
;;

let%expect_test "test string from number (int64) - 1" =
  print @@ as_int64 "123L";
  [%expect {| some 123 |}]
;;

let%expect_test "test string from number (int32) - 1" =
  print @@ as_int32 "123l";
  [%expect {| some 123 |}]
;;

let%expect_test "test string from number (int) - 1" =
  print @@ as_int "123";
  [%expect {| some 123 |}]
;;

let%expect_test "test string from number (int64) - 2" =
  print @@ as_int64 "+123L";
  [%expect {| some 123 |}]
;;

let%expect_test "test string from number (int32) - 2" =
  print @@ as_int32 "+123l";
  [%expect {| some 123 |}]
;;

let%expect_test "test string from number (int) - 2" =
  print @@ as_int "-123";
  [%expect {| some -123 |}]
;;

let%expect_test "test string from number (int64) - 3" =
  print @@ as_int64 "-123L";
  [%expect {| some -123 |}]
;;

let%expect_test "test string from number (int32) - 3" =
  print @@ as_int32 "-123l";
  [%expect {| some -123 |}]
;;

let%expect_test "test string from number (int) - 3" =
  print @@ as_int "-123";
  [%expect {| some -123 |}]
;;

let%expect_test "test string from number (int64) - 4" =
  print @@ as_int64 "-123Lsdasd";
  [%expect {| none |}]
;;

let%expect_test "test string from number (int32) - 4" =
  print @@ as_int32 "-123lsad";
  [%expect {| none |}]
;;

let%expect_test "test string from number (int) - 4" =
  print @@ as_int "-123asdsad";
  [%expect {| none |}]
;;

let%expect_test "test string from number (int64) - 5" =
  print @@ as_int64 "daasdsad";
  [%expect {| none |}]
;;

let%expect_test "test string from number (int32) - 5" =
  print @@ as_int32 "asdsadsads";
  [%expect {| none |}]
;;

let%expect_test "test string from number (int) - 5" =
  print @@ as_int "asdsadsad";
  [%expect {| none |}]
;;
