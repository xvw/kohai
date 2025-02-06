open Kohai_core

let check_day_of_week ~expected = function
  | Ok dt ->
    let dow = Datetime.day_of_week dt in
    if Datetime.equal_day_of_week expected dow
    then
      print_endline
      @@ Format.asprintf "Valid day of week (%a)" Datetime.pp_day_of_week dow
    else
      print_endline
      @@ Format.asprintf
           "Invalid day of week (%a), expected: %a"
           Datetime.pp_day_of_week
           dow
           Datetime.pp_day_of_week
           expected
  | Error err ->
    err
    |> Format.asprintf "Invalid datetime: %a" Rensai.Validation.pp_value_error
    |> print_endline
;;

let dump = function
  | Ok dt -> dt |> Format.asprintf "%a" (Datetime.pp_rfc822 ()) |> print_endline
  | Error err ->
    err
    |> Format.asprintf "Invalid datetime: %a" Rensai.Validation.pp_value_error
    |> print_endline
;;

let%expect_test "day of week of Unix time" =
  let dt = Ok Datetime.unix in
  check_day_of_week ~expected:Datetime.Thu dt;
  [%expect {| Valid day of week (thu) |}]
;;

let%expect_test "day of week of an arbitrary date - 1" =
  let dt = Datetime.(make ~year:2025 ~month:Feb ~day:5 ()) in
  check_day_of_week ~expected:Datetime.Wed dt;
  [%expect {| Valid day of week (wed) |}]
;;

let%expect_test "day of week of an arbitrary date - 2" =
  let dt = Datetime.(make ~year:2025 ~month:Feb ~day:17 ()) in
  check_day_of_week ~expected:Datetime.Mon dt;
  [%expect {| Valid day of week (mon) |}]
;;

let%expect_test "day of week of an arbitrary date - 3" =
  let dt = Datetime.(make ~year:2010 ~month:Nov ~day:24 ()) in
  check_day_of_week ~expected:Datetime.Wed dt;
  [%expect {| Valid day of week (wed) |}]
;;

let%expect_test "begin of day - 1" =
  let test =
    Datetime.make ~time:(12, 0, 0) ~year:2025 ~month:Datetime.Feb ~day:5 ()
    |> Result.map Datetime.begin_of_day
  in
  dump test;
  [%expect {| Wed, 05 Feb 2025 00:00:00 gmt |}]
;;

let%expect_test "end of day - 1" =
  let test =
    Datetime.make ~time:(12, 0, 0) ~year:2025 ~month:Datetime.Feb ~day:5 ()
    |> Result.map Datetime.end_of_day
  in
  dump test;
  [%expect {| Wed, 05 Feb 2025 23:59:59 gmt |}]
;;

let%expect_test "begin of month - 1" =
  let test =
    Datetime.make ~time:(12, 0, 0) ~year:2025 ~month:Datetime.Feb ~day:5 ()
    |> Result.map Datetime.begin_of_month
  in
  dump test;
  [%expect {| Sat, 01 Feb 2025 00:00:00 gmt |}]
;;

let%expect_test "end of month - 1" =
  let test =
    Datetime.make ~time:(12, 0, 0) ~year:2025 ~month:Datetime.Feb ~day:5 ()
    |> Result.map Datetime.end_of_month
  in
  dump test;
  [%expect {| Fri, 28 Feb 2025 23:59:59 gmt |}]
;;

let%expect_test "end of month - 2" =
  let test =
    Datetime.make ~time:(12, 0, 0) ~year:2028 ~month:Datetime.Feb ~day:5 ()
    |> Result.map Datetime.end_of_month
  in
  dump test;
  [%expect {| Tue, 29 Feb 2028 23:59:59 gmt |}]
;;

let%expect_test "begin of year" =
  let test =
    Datetime.make ~time:(12, 0, 0) ~year:2028 ~month:Datetime.Feb ~day:5 ()
    |> Result.map Datetime.begin_of_year
  in
  dump test;
  [%expect {| Sat, 01 Jan 2028 00:00:00 gmt |}]
;;

let%expect_test "end of year" =
  let test =
    Datetime.make ~time:(12, 0, 0) ~year:2028 ~month:Datetime.Feb ~day:5 ()
    |> Result.map Datetime.end_of_year
  in
  dump test;
  [%expect {| Sun, 31 Dec 2028 23:59:59 gmt |}]
;;

let%expect_test "succ year - 1" =
  let test =
    Datetime.make ~time:(12, 0, 0) ~year:2028 ~month:Datetime.Feb ~day:5 ()
    |> Result.map Datetime.succ_year
  in
  dump test;
  [%expect {| Mon, 01 Jan 2029 00:00:00 gmt |}]
;;

let%expect_test "pred year - 1" =
  let test =
    Datetime.make ~time:(12, 0, 0) ~year:2028 ~month:Datetime.Feb ~day:5 ()
    |> Result.map Datetime.pred_year
  in
  dump test;
  [%expect {| Fri, 01 Jan 2027 00:00:00 gmt |}]
;;

let%expect_test "succ month - 1" =
  let test =
    Datetime.make ~time:(12, 0, 0) ~year:2028 ~month:Datetime.Feb ~day:5 ()
    |> Result.map Datetime.succ_month
  in
  dump test;
  [%expect {| Wed, 01 Mar 2028 00:00:00 gmt |}]
;;

let%expect_test "succ month - 2" =
  let test =
    Datetime.make ~time:(12, 0, 0) ~year:2028 ~month:Datetime.Dec ~day:5 ()
    |> Result.map Datetime.succ_month
  in
  dump test;
  [%expect {| Mon, 01 Jan 2029 00:00:00 gmt |}]
;;

let%expect_test "pred month - 1" =
  let test =
    Datetime.make ~time:(12, 0, 0) ~year:2028 ~month:Datetime.Feb ~day:5 ()
    |> Result.map Datetime.pred_month
  in
  dump test;
  [%expect {| Sat, 01 Jan 2028 00:00:00 gmt |}]
;;

let%expect_test "pred month - 2" =
  let test =
    Datetime.make ~time:(12, 0, 0) ~year:2028 ~month:Datetime.Feb ~day:5 ()
    |> Result.map Datetime.pred_month
    |> Result.map Datetime.pred_month
  in
  dump test;
  [%expect {| Wed, 01 Dec 2027 00:00:00 gmt |}]
;;

let%expect_test "succ day - 1" =
  let test =
    Datetime.make ~time:(12, 0, 0) ~year:2028 ~month:Datetime.Feb ~day:5 ()
    |> Result.map Datetime.succ_day
  in
  dump test;
  [%expect {| Sun, 06 Feb 2028 00:00:00 gmt |}]
;;

let%expect_test "succ day - 2" =
  let test =
    Datetime.make ~time:(12, 0, 0) ~year:2028 ~month:Datetime.Feb ~day:28 ()
    |> Result.map Datetime.succ_day
  in
  dump test;
  [%expect {| Tue, 29 Feb 2028 00:00:00 gmt |}]
;;

let%expect_test "succ day - 3" =
  let test =
    Datetime.make ~time:(12, 0, 0) ~year:2028 ~month:Datetime.Feb ~day:28 ()
    |> Result.map Datetime.succ_day
    |> Result.map Datetime.succ_day
  in
  dump test;
  [%expect {| Wed, 01 Mar 2028 00:00:00 gmt |}]
;;

let%expect_test "succ day - 3" =
  let test =
    Datetime.make ~time:(12, 0, 0) ~year:2028 ~month:Datetime.Dec ~day:31 ()
    |> Result.map Datetime.succ_day
  in
  dump test;
  [%expect {| Mon, 01 Jan 2029 00:00:00 gmt |}]
;;

let%expect_test "pred day - 1" =
  let test =
    Datetime.make ~time:(12, 0, 0) ~year:2028 ~month:Datetime.Feb ~day:28 ()
    |> Result.map Datetime.pred_day
  in
  dump test;
  [%expect {| Sun, 27 Feb 2028 00:00:00 gmt |}]
;;

let%expect_test "pred day - 2" =
  let test =
    Datetime.make ~time:(12, 0, 0) ~year:2028 ~month:Datetime.Jan ~day:1 ()
    |> Result.map Datetime.pred_day
  in
  dump test;
  [%expect {| Fri, 31 Dec 2027 00:00:00 gmt |}]
;;
