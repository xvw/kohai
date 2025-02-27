open Kohai_core
open Kohai_model

let mk_date s =
  s
  |> Datetime.from_string
  (* Since it is a test, we can discard
     the error case *)
  |> Result.get_ok
;;

let a_date = mk_date "2025-02-18 10:00:00"

let dump state =
  state
  |> State.to_compact_rensai
  |> Format.asprintf "%a" Rensai.Lang.pp
  |> print_endline
;;

let%expect_test "dump boundaries - 1" =
  let state = State.big_bang () |> State.patch_date_boundaries a_date in
  dump state;
  [%expect
    {| <big_bang: "2025-02-18T10-00-00"; end_of_world: "2025-02-18T10-00-00"> |}]
;;

let%expect_test "dump boundaries - 2" =
  let a_second_date = Datetime.(a_date + min 30) in
  let state =
    State.big_bang ()
    |> State.patch_date_boundaries a_date
    |> State.patch_date_boundaries a_second_date
  in
  dump state;
  [%expect
    {| <big_bang: "2025-02-18T10-00-00"; end_of_world: "2025-02-18T10-30-00"> |}]
;;
