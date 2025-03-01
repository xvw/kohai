let base_filesystem =
  let open Virtfs in
  from_list [ dir ".kohai" [] ]
;;

let base_datetime =
  let open Kohai_core.Datetime in
  match make ~time:(10, 0, 0) ~year:2025 ~month:Mar ~day:1 () with
  | Ok dt -> dt
  | _ -> failwith "Invalid date" (* should not happen. *)
;;

let%expect_test
    {| E2E: Test a simple logging procedure:
       - Set up a supervised directory
       - Store sectors
       - store projects
       - log some stuff 
    |}
  =
  let module F =
    Virtfs.Make (struct
      let fs = base_filesystem
      let now = base_datetime
    end)
  in
  let module H = Kohai_core.Eff.Handler (F) in
  print_endline "Done";
  [%expect {| Done |}]
;;
