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

let%expect_test "pred hour - 1" =
  let test =
    Datetime.make ~time:(12, 0, 0) ~year:2028 ~month:Datetime.Jan ~day:1 ()
    |> Result.map Datetime.pred_hour
  in
  dump test;
  [%expect {| Sat, 01 Jan 2028 11:00:00 gmt |}]
;;

let%expect_test "pred hour - 2" =
  let test =
    Datetime.make ~time:(1, 0, 0) ~year:2028 ~month:Datetime.Jan ~day:1 ()
    |> Result.map Datetime.pred_hour
  in
  dump test;
  [%expect {| Sat, 01 Jan 2028 00:00:00 gmt |}]
;;

let%expect_test "pred hour - 3" =
  let test =
    Datetime.make ~time:(0, 0, 0) ~year:2028 ~month:Datetime.Jan ~day:1 ()
    |> Result.map Datetime.pred_hour
  in
  dump test;
  [%expect {| Fri, 31 Dec 2027 23:00:00 gmt |}]
;;

let%expect_test "pred hour - 4" =
  let test =
    Datetime.make ~time:(0, 0, 0) ~year:2028 ~month:Datetime.Jan ~day:1 ()
    |> Result.map Datetime.pred_hour
  in
  dump test;
  [%expect {| Fri, 31 Dec 2027 23:00:00 gmt |}]
;;

let%expect_test "succ hour - 1" =
  let test =
    Datetime.make ~time:(12, 0, 0) ~year:2028 ~month:Datetime.Jan ~day:1 ()
    |> Result.map Datetime.succ_hour
  in
  dump test;
  [%expect {| Sat, 01 Jan 2028 13:00:00 gmt |}]
;;

let%expect_test "succ hour - 2" =
  let test =
    Datetime.make ~time:(23, 0, 0) ~year:2028 ~month:Datetime.Jan ~day:1 ()
    |> Result.map Datetime.succ_hour
  in
  dump test;
  [%expect {| Sun, 02 Jan 2028 00:00:00 gmt |}]
;;

let%expect_test "succ hour - 3" =
  let test =
    Datetime.make ~time:(23, 0, 0) ~year:2028 ~month:Datetime.Dec ~day:31 ()
    |> Result.map Datetime.succ_hour
  in
  dump test;
  [%expect {| Mon, 01 Jan 2029 00:00:00 gmt |}]
;;

let%expect_test "succ min - 1" =
  let test =
    Datetime.make ~time:(12, 0, 0) ~year:2028 ~month:Datetime.Jan ~day:1 ()
    |> Result.map Datetime.succ_min
  in
  dump test;
  [%expect {| Sat, 01 Jan 2028 12:01:00 gmt |}]
;;

let%expect_test "succ min - 2" =
  let test =
    Datetime.make ~time:(12, 59, 0) ~year:2028 ~month:Datetime.Jan ~day:1 ()
    |> Result.map Datetime.succ_min
  in
  dump test;
  [%expect {| Sat, 01 Jan 2028 13:00:00 gmt |}]
;;

let%expect_test "succ min - 3" =
  let test =
    Datetime.make ~time:(23, 59, 0) ~year:2028 ~month:Datetime.Jan ~day:1 ()
    |> Result.map Datetime.succ_min
  in
  dump test;
  [%expect {| Sun, 02 Jan 2028 00:00:00 gmt |}]
;;

let%expect_test "succ min - 4" =
  let test =
    Datetime.make ~time:(23, 59, 0) ~year:2028 ~month:Datetime.Dec ~day:31 ()
    |> Result.map Datetime.succ_min
  in
  dump test;
  [%expect {| Mon, 01 Jan 2029 00:00:00 gmt |}]
;;

let%expect_test "pred min - 1" =
  let test =
    Datetime.make ~time:(12, 2, 0) ~year:2028 ~month:Datetime.Jan ~day:1 ()
    |> Result.map Datetime.pred_min
  in
  dump test;
  [%expect {| Sat, 01 Jan 2028 12:01:00 gmt |}]
;;

let%expect_test "pred min - 2" =
  let test =
    Datetime.make ~time:(12, 2, 0) ~year:2028 ~month:Datetime.Jan ~day:1 ()
    |> Result.map Datetime.pred_min
    |> Result.map Datetime.pred_min
    |> Result.map Datetime.pred_min
  in
  dump test;
  [%expect {| Sat, 01 Jan 2028 11:59:00 gmt |}]
;;

let%expect_test "pred min - 3" =
  let test =
    Datetime.make ~time:(0, 2, 0) ~year:2028 ~month:Datetime.Jan ~day:1 ()
    |> Result.map Datetime.pred_min
    |> Result.map Datetime.pred_min
    |> Result.map Datetime.pred_min
    |> Result.map Datetime.pred_min
  in
  dump test;
  [%expect {| Fri, 31 Dec 2027 23:58:00 gmt |}]
;;

let%expect_test "succ sec - 1" =
  let test =
    Datetime.make ~time:(23, 59, 0) ~year:2028 ~month:Datetime.Dec ~day:31 ()
    |> Result.map Datetime.succ_sec
  in
  dump test;
  [%expect {| Sun, 31 Dec 2028 23:59:01 gmt |}]
;;

let%expect_test "succ sec - 2" =
  let test =
    Datetime.make ~time:(23, 59, 0) ~year:2028 ~month:Datetime.Dec ~day:31 ()
    |> Result.map Datetime.succ_sec
    |> Result.map Datetime.succ_sec
    |> Result.map Datetime.succ_sec
    |> Result.map Datetime.succ_sec
  in
  dump test;
  [%expect {| Sun, 31 Dec 2028 23:59:04 gmt |}]
;;

let%expect_test "succ sec - 3" =
  let test =
    Datetime.make ~time:(23, 59, 59) ~year:2028 ~month:Datetime.Dec ~day:30 ()
    |> Result.map Datetime.succ_sec
  in
  dump test;
  [%expect {| Sun, 31 Dec 2028 00:00:00 gmt |}]
;;

let%expect_test "succ sec - 4" =
  let test =
    Datetime.make ~time:(23, 59, 59) ~year:2028 ~month:Datetime.Dec ~day:31 ()
    |> Result.map Datetime.succ_sec
  in
  dump test;
  [%expect {| Mon, 01 Jan 2029 00:00:00 gmt |}]
;;

let%expect_test "pred sec - 1" =
  let test =
    Datetime.make ~time:(23, 59, 0) ~year:2028 ~month:Datetime.Dec ~day:31 ()
    |> Result.map Datetime.pred_sec
  in
  dump test;
  [%expect {| Sun, 31 Dec 2028 23:58:59 gmt |}]
;;

let%expect_test "pred sec - 2" =
  let test =
    Datetime.make ~time:(23, 59, 0) ~year:2028 ~month:Datetime.Dec ~day:31 ()
    |> Result.map Datetime.pred_sec
    |> Result.map Datetime.pred_sec
    |> Result.map Datetime.pred_sec
    |> Result.map Datetime.pred_sec
    |> Result.map Datetime.pred_sec
    |> Result.map Datetime.pred_sec
    |> Result.map Datetime.pred_sec
  in
  dump test;
  [%expect {| Sun, 31 Dec 2028 23:58:53 gmt |}]
;;

let%expect_test "pred sec - 3" =
  let test =
    Datetime.make ~time:(0, 0, 0) ~year:2028 ~month:Datetime.Dec ~day:31 ()
    |> Result.map Datetime.pred_sec
  in
  dump test;
  [%expect {| Sat, 30 Dec 2028 23:59:59 gmt |}]
;;

let%expect_test "pred sec - 4" =
  let test =
    Datetime.make ~time:(0, 0, 0) ~year:2028 ~month:Datetime.Jan ~day:1 ()
    |> Result.map Datetime.pred_sec
  in
  dump test;
  [%expect {| Fri, 31 Dec 2027 23:59:59 gmt |}]
;;

let%expect_test "pred sec - 5" =
  let test =
    Datetime.make ~time:(0, 0, 0) ~year:2028 ~month:Datetime.Mar ~day:1 ()
    |> Result.map Datetime.pred_sec
  in
  dump test;
  [%expect {| Tue, 29 Feb 2028 23:59:59 gmt |}]
;;

let%expect_test "Arithmetic operation - 1" =
  let test =
    Datetime.make ~time:(21, 12, 33) ~year:2025 ~month:Datetime.Mar ~day:2 ()
    |> Result.map (fun dt ->
      let open Datetime in
      dt + year 2 + month 3 + day 2 + hour 3 + min 6 + sec 7 + day 2)
  in
  dump test;
  [%expect {| Mon, 05 Apr 2027 00:00:00 gmt |}]
;;

let%expect_test "begin of week - 1" =
  let test =
    Datetime.make ~time:(22, 0, 0) ~year:2025 ~month:Datetime.Feb ~day:3 ()
    |> Result.map Datetime.begin_of_week
  in
  dump test;
  [%expect {| Mon, 03 Feb 2025 00:00:00 gmt |}]
;;

let%expect_test "begin of week - 2" =
  let test =
    Datetime.make ~time:(22, 0, 0) ~year:2025 ~month:Datetime.Feb ~day:4 ()
    |> Result.map Datetime.begin_of_week
  in
  dump test;
  [%expect {| Mon, 03 Feb 2025 00:00:00 gmt |}]
;;

let%expect_test "begin of week - 3" =
  let test =
    Datetime.make ~time:(22, 0, 0) ~year:2025 ~month:Datetime.Feb ~day:5 ()
    |> Result.map Datetime.begin_of_week
  in
  dump test;
  [%expect {| Mon, 03 Feb 2025 00:00:00 gmt |}]
;;

let%expect_test "begin of week - 4" =
  let test =
    Datetime.make ~time:(22, 0, 0) ~year:2025 ~month:Datetime.Feb ~day:6 ()
    |> Result.map Datetime.begin_of_week
  in
  dump test;
  [%expect {| Mon, 03 Feb 2025 00:00:00 gmt |}]
;;

let%expect_test "begin of week - 5" =
  let test =
    Datetime.make ~time:(22, 0, 0) ~year:2025 ~month:Datetime.Feb ~day:7 ()
    |> Result.map Datetime.begin_of_week
  in
  dump test;
  [%expect {| Mon, 03 Feb 2025 00:00:00 gmt |}]
;;

let%expect_test "begin of week - 6" =
  let test =
    Datetime.make ~time:(22, 0, 0) ~year:2025 ~month:Datetime.Feb ~day:8 ()
    |> Result.map Datetime.begin_of_week
  in
  dump test;
  [%expect {| Mon, 03 Feb 2025 00:00:00 gmt |}]
;;

let%expect_test "begin of week - 7" =
  let test =
    Datetime.make ~time:(22, 0, 0) ~year:2025 ~month:Datetime.Feb ~day:9 ()
    |> Result.map Datetime.begin_of_week
  in
  dump test;
  [%expect {| Mon, 03 Feb 2025 00:00:00 gmt |}]
;;

let%expect_test "begin of week - 8" =
  let test =
    Datetime.make ~time:(22, 0, 0) ~year:2025 ~month:Datetime.Feb ~day:10 ()
    |> Result.map Datetime.begin_of_week
  in
  dump test;
  [%expect {| Mon, 10 Feb 2025 00:00:00 gmt |}]
;;

let%expect_test "begin of week - 9" =
  let test =
    Datetime.make ~time:(22, 0, 0) ~year:2025 ~month:Datetime.Feb ~day:11 ()
    |> Result.map Datetime.begin_of_week
  in
  dump test;
  [%expect {| Mon, 10 Feb 2025 00:00:00 gmt |}]
;;

let%expect_test "end of week - 1" =
  let test =
    Datetime.make ~time:(22, 0, 0) ~year:2025 ~month:Datetime.Feb ~day:3 ()
    |> Result.map Datetime.end_of_week
  in
  dump test;
  [%expect {| Sun, 09 Feb 2025 23:59:59 gmt |}]
;;

let%expect_test "end of week - 2" =
  let test =
    Datetime.make ~time:(22, 0, 0) ~year:2025 ~month:Datetime.Feb ~day:4 ()
    |> Result.map Datetime.end_of_week
  in
  dump test;
  [%expect {| Sun, 09 Feb 2025 23:59:59 gmt |}]
;;

let%expect_test "end of week - 3" =
  let test =
    Datetime.make ~time:(22, 0, 0) ~year:2025 ~month:Datetime.Feb ~day:5 ()
    |> Result.map Datetime.end_of_week
  in
  dump test;
  [%expect {| Sun, 09 Feb 2025 23:59:59 gmt |}]
;;

let%expect_test "end of week - 4" =
  let test =
    Datetime.make ~time:(22, 0, 0) ~year:2025 ~month:Datetime.Feb ~day:6 ()
    |> Result.map Datetime.end_of_week
  in
  dump test;
  [%expect {| Sun, 09 Feb 2025 23:59:59 gmt |}]
;;

let%expect_test "end of week - 5" =
  let test =
    Datetime.make ~time:(22, 0, 0) ~year:2025 ~month:Datetime.Feb ~day:7 ()
    |> Result.map Datetime.end_of_week
  in
  dump test;
  [%expect {| Sun, 09 Feb 2025 23:59:59 gmt |}]
;;

let%expect_test "end of week - 6" =
  let test =
    Datetime.make ~time:(22, 0, 0) ~year:2025 ~month:Datetime.Feb ~day:8 ()
    |> Result.map Datetime.end_of_week
  in
  dump test;
  [%expect {| Sun, 09 Feb 2025 23:59:59 gmt |}]
;;

let%expect_test "end of week - 7" =
  let test =
    Datetime.make ~time:(22, 0, 0) ~year:2025 ~month:Datetime.Feb ~day:9 ()
    |> Result.map Datetime.end_of_week
  in
  dump test;
  [%expect {| Sun, 09 Feb 2025 23:59:59 gmt |}]
;;

let%expect_test "end of week - 8" =
  let test =
    Datetime.make ~time:(22, 0, 0) ~year:2025 ~month:Datetime.Feb ~day:10 ()
    |> Result.map Datetime.end_of_week
  in
  dump test;
  [%expect {| Sun, 16 Feb 2025 23:59:59 gmt |}]
;;

let%expect_test "end of week - 9" =
  let test =
    Datetime.make ~time:(22, 0, 0) ~year:2025 ~month:Datetime.Feb ~day:11 ()
    |> Result.map Datetime.end_of_week
  in
  dump test;
  [%expect {| Sun, 16 Feb 2025 23:59:59 gmt |}]
;;

let%expect_test "succ week - 1" =
  let test =
    Datetime.make ~time:(22, 0, 0) ~year:2025 ~month:Datetime.Feb ~day:11 ()
    |> Result.map Datetime.succ_week
  in
  dump test;
  [%expect {| Mon, 17 Feb 2025 00:00:00 gmt |}]
;;

let%expect_test "succ week - 2" =
  let test =
    Datetime.make ~time:(22, 0, 0) ~year:2025 ~month:Datetime.Dec ~day:30 ()
    |> Result.map Datetime.succ_week
  in
  dump test;
  [%expect {| Mon, 05 Jan 2026 00:00:00 gmt |}]
;;

let%expect_test "pred week - 1" =
  let test =
    Datetime.make ~time:(22, 0, 0) ~year:2025 ~month:Datetime.Dec ~day:30 ()
    |> Result.map Datetime.pred_week
  in
  dump test;
  [%expect {| Mon, 22 Dec 2025 00:00:00 gmt |}]
;;
