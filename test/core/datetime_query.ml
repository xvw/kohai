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
    query
    |> Datetime.Query.resolve current_time
    |> Format.asprintf "%a" (Datetime.pp ())
    |> print_endline
  | _ -> failwith "datetime_query_dump: Test fail"
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

let%expect_test "absolute - 1" =
  "2022-02-10" |> Datetime.Query.from_string |> dump;
  [%expect {| 2022-02-10T00-00-00 |}]
;;

let%expect_test "absolute - 2" =
  "2022-02-10T10:15:30" |> Datetime.Query.from_string |> dump;
  [%expect {| 2022-02-10T10-15-30 |}]
;;

(* let%expect_test "absolute - 3" = *)
(*   "2022-02-10 10:15" |> Datetime.Query.from_string |> dump; *)
(*   [%expect {| 2022-02-10T10-15-30 |}] *)
(* ;; *)
