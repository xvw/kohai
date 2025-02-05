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
    |> Format.asprintf "%a" Rensai.Validation.pp_value_error
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
