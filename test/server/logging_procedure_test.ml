open Util

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
  let step = step (module H) in
  let id = ref 0 in
  let _ =
    {|
     As no supervised directory has been submitted, retrieving the
     supervised directory should return `null`.
    |}
  in
  let () =
    step
      ~desc:
        {| As no supervised directory has been submitted,
         retrieving the supervised directory should return `null`.
        |}
      ~should_fail:false
      ~id
      call_supervise_get
  in
  [%expect {| [OK]: <id: 0; jsonrpc: "2.0"; result: null> |}];
  let () =
    step
      ~desc:
        {|Assigns a non-existing supervision directory.
             So the request should fail.|}
      ~should_fail:true
      ~id
      (call_supervise ~path:"/.logging")
  in
  [%expect
    {|
    [OK]: <error:
            <code: -32001; data: "The given directory does not exists";
              input:
               "{"jsonrpc": "2.0", "method": "kohai/supervision/set", "id": 1, "params": "/.logging"}";
              message: "Server error">;
            id: 1; jsonrpc: "2.0">
    |}];
  let () =
    step
      ~desc:{|Assigns an existing supervision directory.|}
      ~should_fail:false
      ~id
      (call_supervise ~path:"/.kohai")
  in
  [%expect {| [OK]: <id: 2; jsonrpc: "2.0"; result: "/.kohai"> |}];
  let () =
    step
      ~desc:{|Get the list of sector (should be empty).|}
      ~should_fail:false
      ~id
      call_sector_list
  in
  [%expect {| [OK]: <id: 3; jsonrpc: "2.0"; result: []> |}];
  let () =
    step
      ~desc:{|Get the list of project (should be empty).|}
      ~should_fail:false
      ~id
      call_project_list
  in
  [%expect {| [OK]: <id: 4; jsonrpc: "2.0"; result: []> |}];
  print_endline "[DONE]";
  [%expect {| [DONE] |}]
;;
