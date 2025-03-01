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
  [%expect {| [DONE]: <id: 0; jsonrpc: "2.0"; result: null> |}];
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
    [DONE]: <error:
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
  [%expect {| [DONE]: <id: 2; jsonrpc: "2.0"; result: "/.kohai"> |}];
  let () =
    step
      ~desc:{|Get the list of sector (should be empty).|}
      ~should_fail:false
      ~id
      call_sector_list
  in
  [%expect {| [DONE]: <id: 3; jsonrpc: "2.0"; result: []> |}];
  let () =
    step
      ~desc:{|Get the list of project (should be empty).|}
      ~should_fail:false
      ~id
      call_project_list
  in
  [%expect {| [DONE]: <id: 4; jsonrpc: "2.0"; result: []> |}];
  let () =
    step
      ~desc:{|Save a sector.|}
      ~should_fail:false
      ~id
      (call_sector_save
         ~name:"programming"
         ~desc:"Category related to programming")
  in
  [%expect
    {|
    [DONE]: <id: 5; jsonrpc: "2.0";
              result:
               [<counter: 0; description: "Category related to programming";
                  name: "programming">]>
    |}];
  let () =
    step
      ~desc:{|Save an other sector.|}
      ~should_fail:false
      ~id
      (call_sector_save ~name:"visual" ?desc:None)
  in
  [%expect
    {|
    [DONE]: <id: 6; jsonrpc: "2.0";
              result:
               [<counter: 0; description: "Category related to programming";
                  name: "programming">,
                <counter: 0; description: null; name: "visual">]>
    |}];
  let () =
    step
      ~desc:{|Patch an existing sector without updates.|}
      ~should_fail:false
      ~id
      (call_sector_save ~name:"programming" ?desc:None)
  in
  [%expect
    {|
    [DONE]: <id: 7; jsonrpc: "2.0";
              result:
               [<counter: 0; description: "Category related to programming";
                  name: "programming">,
                <counter: 0; description: null; name: "visual">]>
    |}];
  let () =
    step
      ~desc:{|Patch an existing sector with updates.|}
      ~should_fail:false
      ~id
      (call_sector_save ~name:"visual" ~desc:"A description")
  in
  [%expect
    {|
    [DONE]: <id: 8; jsonrpc: "2.0";
              result:
               [<counter: 0; description: "Category related to programming";
                  name: "programming">,
                <counter: 0; description: "A description"; name: "visual">]>
    |}];
  let () =
    step
      ~desc:{|Get the list of sector (should be filled with 2 entries).|}
      ~should_fail:false
      ~id
      call_sector_list
  in
  [%expect
    {|
    [DONE]: <id: 9; jsonrpc: "2.0";
              result:
               [<counter: 0; description: "Category related to programming";
                  name: "programming">,
                <counter: 0; description: "A description"; name: "visual">]>
            |}];
  let () =
    step
      ~desc:{|Save a project.|}
      ~should_fail:false
      ~id
      (call_project_save ~name:"kohai" ~desc:"An opinionated timetracker")
  in
  [%expect
    {|
    [DONE]: <id: 10; jsonrpc: "2.0";
              result:
               [<counter: 0; description: "An opinionated timetracker";
                  name: "kohai">]>
    |}];
  let () =
    step
      ~desc:{|Save an other project.|}
      ~should_fail:false
      ~id
      (call_project_save ~name:"capsule" ?desc:None)
  in
  [%expect
    {|
    [DONE]: <id: 11; jsonrpc: "2.0";
              result:
               [<counter: 0; description: null; name: "capsule">,
                <counter: 0; description: "An opinionated timetracker";
                  name: "kohai">]>
     |}];
  let () =
    step
      ~desc:{|Patch project without updates.|}
      ~should_fail:false
      ~id
      (call_project_save ~name:"kohai" ?desc:None)
  in
  [%expect
    {|
    [DONE]: <id: 12; jsonrpc: "2.0";
              result:
               [<counter: 0; description: null; name: "capsule">,
                <counter: 0; description: "An opinionated timetracker";
                  name: "kohai">]>
     |}];
  let () =
    step
      ~desc:{|Patch project with updates.|}
      ~should_fail:false
      ~id
      (call_project_save
         ~name:"capsule"
         ~desc:"My personnal website built with OCaml")
  in
  [%expect
    {|
    [DONE]: <id: 13; jsonrpc: "2.0";
              result:
               [<counter: 0;
                  description: "My personnal website built with OCaml";
                  name: "capsule">,
                <counter: 0; description: "An opinionated timetracker";
                  name: "kohai">]>
     |}];
  let () =
    step
      ~desc:{|Get the list of project (should be filled with 2 entries).|}
      ~should_fail:false
      ~id
      call_project_list
  in
  [%expect
    {|
    [DONE]: <id: 14; jsonrpc: "2.0";
              result:
               [<counter: 0;
                  description: "My personnal website built with OCaml";
                  name: "capsule">,
                <counter: 0; description: "An opinionated timetracker";
                  name: "kohai">]>
     |}];
  let () =
    step
      ~desc:{|Save a project (in order to be deleted).|}
      ~should_fail:false
      ~id
      (call_project_save ~name:"preface" ~desc:"A library")
  in
  [%expect
    {|
    [DONE]: <id: 15; jsonrpc: "2.0";
              result:
               [<counter: 0;
                  description: "My personnal website built with OCaml";
                  name: "capsule">,
                <counter: 0; description: "An opinionated timetracker";
                  name: "kohai">,
                <counter: 0; description: "A library"; name: "preface">]>
     |}];
  let () =
    step
      ~desc:{|Delete the project.|}
      ~should_fail:false
      ~id
      (call_project_delete ~name:"preface")
  in
  [%expect
    {|
    [DONE]: <id: 16; jsonrpc: "2.0";
              result:
               [<counter: 0;
                  description: "My personnal website built with OCaml";
                  name: "capsule">,
                <counter: 0; description: "An opinionated timetracker";
                  name: "kohai">]>
     |}];
  let () =
    step
      ~desc:{|Save a sector (in order to be deleted).|}
      ~should_fail:false
      ~id
      (call_sector_save ~name:"painting" ~desc:"desc")
  in
  [%expect
    {|
    [DONE]: <id: 17; jsonrpc: "2.0";
              result:
               [<counter: 0; description: "desc"; name: "painting">,
                <counter: 0; description: "Category related to programming";
                  name: "programming">,
                <counter: 0; description: "A description"; name: "visual">]>
    |}];
  let () =
    step
      ~desc:{|Delete the sector.|}
      ~should_fail:false
      ~id
      (call_sector_delete ~name:"painting")
  in
  [%expect
    {|
    [DONE]: <id: 18; jsonrpc: "2.0";
              result:
               [<counter: 0; description: "Category related to programming";
                  name: "programming">,
                <counter: 0; description: "A description"; name: "visual">]>
    |}];
  print_endline "[DONE]";
  [%expect {| [DONE] |}]
;;
