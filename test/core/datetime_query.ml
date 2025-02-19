open Kohai_core

let current_time =
  "2025-02-18 10:00:00"
  |> Datetime.from_string
  (* Since it is a test, we can discard
     the error case *)
  |> Result.get_ok
;;

let dump ?(should_fail = false) = function
  | Error _ when should_fail -> print_endline "OK: Parsing_failure"
  | Ok query when not should_fail ->
    Some query
    |> Datetime.Query.resolve current_time
    |> Format.asprintf "%a" (Datetime.pp ())
    |> print_endline
  | Ok _ -> print_endline "FAILURE: Test should fail"
  | Error err ->
    err
    |> Format.asprintf "FAILURE: %a" Rensai.Validation.pp_value_error
    |> print_endline
;;

let%expect_test "parse-failure - 1" =
  "foobar" |> Datetime.Query.from_string |> dump ~should_fail:true;
  [%expect {| OK: Parsing_failure |}]
;;

let%expect_test "now - 1" =
  "now" |> Datetime.Query.from_string |> dump;
  [%expect {| 2025-02-18T10-00-00 |}]
;;

let%expect_test "now - 2" =
  "  now  " |> Datetime.Query.from_string |> dump;
  [%expect {| 2025-02-18T10-00-00 |}]
;;

let%expect_test "now - 3" =
  "  NoW" |> Datetime.Query.from_string |> dump;
  [%expect {| 2025-02-18T10-00-00 |}]
;;

let%expect_test "now - 4" =
  "  NoW    " |> Datetime.Query.from_string |> dump;
  [%expect {| 2025-02-18T10-00-00 |}]
;;

let%expect_test "absolute - 1" =
  "2022-02-10" |> Datetime.Query.from_string |> dump;
  [%expect {| 2022-02-10T00-00-00 |}]
;;

let%expect_test "absolute - 2" =
  "2022-02-10T10:15:30" |> Datetime.Query.from_string |> dump;
  [%expect {| 2022-02-10T10-15-30 |}]
;;

let%expect_test "at - 1" =
  "12:18:37" |> Datetime.Query.from_string |> dump;
  [%expect {| 2025-02-18T12-18-37 |}]
;;

let%expect_test "at - 2" =
  "at 12:18:37" |> Datetime.Query.from_string |> dump;
  [%expect {| 2025-02-18T12-18-37 |}]
;;

let%expect_test "at - 3" =
  " AT 12 18-37 " |> Datetime.Query.from_string |> dump;
  [%expect {| 2025-02-18T12-18-37 |}]
;;

let%expect_test "at - 4" =
  " AT 2:1:3 " |> Datetime.Query.from_string |> dump;
  [%expect {| 2025-02-18T02-01-03 |}]
;;

let%expect_test "at - 5" =
  " AT 21:1:3 " |> Datetime.Query.from_string |> dump;
  [%expect {| 2025-02-18T21-01-03 |}]
;;

let%expect_test "at - 6" =
  " AT 27:1:63 " |> Datetime.Query.from_string |> dump ~should_fail:true;
  [%expect {| OK: Parsing_failure |}]
;;

let%expect_test "at - 7" =
  "12:18" |> Datetime.Query.from_string |> dump;
  [%expect {| 2025-02-18T12-18-00 |}]
;;

let%expect_test "at - 8" =
  "12" |> Datetime.Query.from_string |> dump;
  [%expect {| 2025-02-18T12-00-00 |}]
;;

let%expect_test "at - 9" =
  "12 " |> Datetime.Query.from_string |> dump;
  [%expect {| 2025-02-18T12-00-00 |}]
;;

let%expect_test "at - 10" =
  "12h " |> Datetime.Query.from_string |> dump;
  [%expect {| 2025-02-18T12-00-00 |}]
;;

let%expect_test "at - 11" =
  "22h43" |> Datetime.Query.from_string |> dump;
  [%expect {| 2025-02-18T22-43-00 |}]
;;

let%expect_test "at - 12" =
  "at   15h59" |> Datetime.Query.from_string |> dump;
  [%expect {| 2025-02-18T15-59-00 |}]
;;

let%expect_test "at - 13" =
  "at   15h59 12" |> Datetime.Query.from_string |> dump;
  [%expect {| 2025-02-18T15-59-12 |}]
;;

let%expect_test "at - 14" =
  "at   15h59m12" |> Datetime.Query.from_string |> dump;
  [%expect {| 2025-02-18T15-59-12 |}]
;;
