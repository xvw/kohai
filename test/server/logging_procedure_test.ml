open Kohai_core
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
       - Manipulation of sectors and project
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
      ~desc:
        {| As no supervised directory has been submitted,
         retrieving the supervised directory should return `null`.
        |}
      ~should_fail:false
      ~id
      call_supervise_get
  in
  [%expect {| [DONE]: <id: 2; jsonrpc: "2.0"; result: null> |}];
  let () =
    step
      ~desc:{|Fetch the state that should fail since there is no supervision|}
      ~should_fail:true
      ~id
      call_state_get
  in
  [%expect
    {|
    [DONE]: <error:
              <code: -32000;
                data: "No supervised directory for the current session";
                input: "{"jsonrpc": "2.0", "method": "kohai/state/get", "id": 3}";
                message: "Server error">;
              id: 3; jsonrpc: "2.0">
    |}];
  let () =
    step
      ~desc:{|Assigns an existing supervision directory.|}
      ~should_fail:false
      ~id
      (call_supervise ~path:"/.kohai")
  in
  [%expect {| [DONE]: <id: 4; jsonrpc: "2.0"; result: "/.kohai"> |}];
  let () =
    step
      ~desc:{|Get the list of sector (should be empty).|}
      ~should_fail:false
      ~id
      call_sector_list
  in
  [%expect {| [DONE]: <id: 5; jsonrpc: "2.0"; result: []> |}];
  let () =
    step
      ~desc:{|Get the list of project (should be empty).|}
      ~should_fail:false
      ~id
      call_project_list
  in
  [%expect {| [DONE]: <id: 6; jsonrpc: "2.0"; result: []> |}];
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
    [DONE]: <id: 7; jsonrpc: "2.0";
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
    [DONE]: <id: 8; jsonrpc: "2.0";
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
    [DONE]: <id: 9; jsonrpc: "2.0";
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
    [DONE]: <id: 10; jsonrpc: "2.0";
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
    [DONE]: <id: 11; jsonrpc: "2.0";
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
    [DONE]: <id: 12; jsonrpc: "2.0";
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
    [DONE]: <id: 13; jsonrpc: "2.0";
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
    [DONE]: <id: 14; jsonrpc: "2.0";
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
    [DONE]: <id: 15; jsonrpc: "2.0";
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
      ~desc:{|Save a project (in order to be deleted).|}
      ~should_fail:false
      ~id
      (call_project_save ~name:"preface" ~desc:"A library")
  in
  [%expect
    {|
    [DONE]: <id: 17; jsonrpc: "2.0";
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
    [DONE]: <id: 18; jsonrpc: "2.0";
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
    [DONE]: <id: 19; jsonrpc: "2.0";
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
    [DONE]: <id: 20; jsonrpc: "2.0";
              result:
               [<counter: 0; description: "Category related to programming";
                  name: "programming">,
                <counter: 0; description: "A description"; name: "visual">]>
    |}];
  let () =
    step
      ~desc:{|Get the transient log list (should be empty).|}
      ~should_fail:false
      ~id
      call_transient_log_list
  in
  [%expect {| [DONE]: <id: 21; jsonrpc: "2.0"; result: []> |}];
  let () =
    step
      ~desc:
        {|Store a transient log with an existing project and an existing sector.|}
      ~should_fail:false
      ~id
      (call_transient_log_record
         ?date_query:None
         ~project:"kohai"
         ~sector:"programming"
         ~label:"A first transient log!")
  in
  [%expect
    {|
    [DONE]: <id: 22; jsonrpc: "2.0";
              result:
               <all:
                 [<duration: null; duration_repr: null; index: 0;
                    label: "A first transient log!"; links: []; meta: [];
                    project: "kohai"; sector: "programming";
                    start_date: "2025-03-01T11-00-00";
                    start_date_repr: "Today at 11:00:00">];
                 inserted:
                  <duration: null; index: -1; label: "A first transient log!";
                    links: []; meta: []; project: "kohai"; sector: "programming";
                    start_date: "2025-03-01T11-00-00">;
                 outdated: []>>
    |}];
  let () =
    step
      ~desc:{|Get the transient log list (should be filled with one element).|}
      ~should_fail:false
      ~id
      call_transient_log_list
  in
  [%expect
    {|
    [DONE]: <id: 23; jsonrpc: "2.0";
              result:
               [<duration: null; duration_repr: null; index: 0;
                  label: "A first transient log!"; links: []; meta: [];
                  project: "kohai"; sector: "programming";
                  start_date: "2025-03-01T11-00-00";
                  start_date_repr: "Today at 11:00:00">]>
    |}];
  let () = F.manip_time Datetime.succ_hour in
  let () =
    step
      ~desc:
        {|Store an other transient log without project and an non-existing sector.|}
      ~should_fail:false
      ~id
      (call_transient_log_record
         ?date_query:None
         ?project:None
         ~sector:"a-new-sector"
         ~label:"A first transient log!")
  in
  [%expect
    {|
    [DONE]: <id: 24; jsonrpc: "2.0";
              result:
               <all:
                 [<duration: null; duration_repr: null; index: 0;
                    label: "A first transient log!"; links: []; meta: [];
                    project: "kohai"; sector: "programming";
                    start_date: "2025-03-01T11-00-00";
                    start_date_repr: "Today at 11:00:00">,
                  <duration: null; duration_repr: null; index: 1;
                    label: "A first transient log!"; links: []; meta: [];
                    project: null; sector: "a-new-sector";
                    start_date: "2025-03-01T12-00-00";
                    start_date_repr: "Today at 12:00:00">];
                 inserted:
                  <duration: null; index: -1; label: "A first transient log!";
                    links: []; meta: []; project: null; sector: "a-new-sector";
                    start_date: "2025-03-01T12-00-00">;
                 outdated:
                  [<computed_duration: 60;
                     record:
                      <duration: null; duration_repr: null; index: 0;
                        label: "A first transient log!"; links: []; meta: [];
                        project: "kohai"; sector: "programming";
                        start_date: "2025-03-01T11-00-00";
                        start_date_repr: "Today at 11:00:00">>]>>
    |}];
  let () =
    step
      ~desc:{|Get the list of sector (should be filled with 3 entries).|}
      ~should_fail:false
      ~id
      call_sector_list
  in
  [%expect
    {|
    [DONE]: <id: 25; jsonrpc: "2.0";
              result:
               [<counter: 0; description: null; name: "a-new-sector">,
                <counter: 0; description: "Category related to programming";
                  name: "programming">,
                <counter: 0; description: "A description"; name: "visual">]>
    |}];
  let () = F.manip_time Datetime.succ_hour in
  let () =
    step
      ~desc:{|Close a the log (indexed-0) with the default duration.|}
      ~should_fail:false
      ~id
      (call_transient_log_stop_recording ~index:0 ?duration:None)
  in
  [%expect
    {|
    [DONE]: <id: 26; jsonrpc: "2.0";
              result:
               <all:
                 [<duration: 7200; duration_repr: "2h"; index: 0;
                    label: "A first transient log!"; links: []; meta: [];
                    project: "kohai"; sector: "programming";
                    start_date: "2025-03-01T11-00-00";
                    start_date_repr: "Today at 11:00:00">,
                  <duration: null; duration_repr: null; index: 1;
                    label: "A first transient log!"; links: []; meta: [];
                    project: null; sector: "a-new-sector";
                    start_date: "2025-03-01T12-00-00";
                    start_date_repr: "Today at 12:00:00">];
                 inserted: null; outdated: []>>
    |}];
  let () =
    step
      ~desc:{|Rewrite the 0-indexed log.|}
      ~should_fail:false
      ~id
      (call_transient_log_rewrite
         ~index:0
         ~sector:"programming"
         ?date_query:None
         ~project:"kohai"
         ~label:"A new label !")
  in
  [%expect
    {|
    [DONE]: <id: 27; jsonrpc: "2.0";
              result:
               <all:
                 [<duration: null; duration_repr: null; index: 0;
                    label: "A first transient log!"; links: []; meta: [];
                    project: null; sector: "a-new-sector";
                    start_date: "2025-03-01T12-00-00";
                    start_date_repr: "Today at 12:00:00">,
                  <duration: null; duration_repr: null; index: 1;
                    label: "A new label !"; links: []; meta: [];
                    project: "kohai"; sector: "programming";
                    start_date: "2025-03-01T13-00-00";
                    start_date_repr: "Today at 13:00:00">];
                 inserted: null; outdated: []>>
    |}];
  let () = F.manip_time Datetime.succ_hour in
  let () =
    step
      ~desc:{|Close a the log (indexed-1) with a given duration.|}
      ~should_fail:false
      ~id
      (call_transient_log_stop_recording ~index:1 ~duration:25)
  in
  [%expect
    {|
    [DONE]: <id: 28; jsonrpc: "2.0";
              result:
               <all:
                 [<duration: null; duration_repr: null; index: 0;
                    label: "A first transient log!"; links: []; meta: [];
                    project: null; sector: "a-new-sector";
                    start_date: "2025-03-01T12-00-00";
                    start_date_repr: "Today at 12:00:00">,
                  <duration: 1500; duration_repr: "25m"; index: 1;
                    label: "A new label !"; links: []; meta: [];
                    project: "kohai"; sector: "programming";
                    start_date: "2025-03-01T13-00-00";
                    start_date_repr: "Today at 13:00:00">];
                 inserted: null; outdated: []>>
    |}];
  let () =
    step
      ~desc:{|Add meta into index-1-log.|}
      ~should_fail:false
      ~id
      (call_transient_log_add_meta ~index:1 ~key:"Foo" ~value:"Bar")
  in
  [%expect
    {|
    [DONE]: <id: 29; jsonrpc: "2.0";
              result:
               <all:
                 [<duration: null; duration_repr: null; index: 0;
                    label: "A first transient log!"; links: []; meta: [];
                    project: null; sector: "a-new-sector";
                    start_date: "2025-03-01T12-00-00";
                    start_date_repr: "Today at 12:00:00">,
                  <duration: 1500; duration_repr: "25m"; index: 1;
                    label: "A new label !"; links: [];
                    meta: [<key: "Foo"; value: "Bar">]; project: "kohai";
                    sector: "programming"; start_date: "2025-03-01T13-00-00";
                    start_date_repr: "Today at 13:00:00">];
                 inserted: null; outdated: []>>
    |}];
  let () =
    step
      ~desc:{|Add meta into index-1-log.|}
      ~should_fail:false
      ~id
      (call_transient_log_add_meta ~index:1 ~key:"a meta" ~value:"hehehe")
  in
  [%expect
    {|
    [DONE]: <id: 30; jsonrpc: "2.0";
              result:
               <all:
                 [<duration: null; duration_repr: null; index: 0;
                    label: "A first transient log!"; links: []; meta: [];
                    project: null; sector: "a-new-sector";
                    start_date: "2025-03-01T12-00-00";
                    start_date_repr: "Today at 12:00:00">,
                  <duration: 1500; duration_repr: "25m"; index: 1;
                    label: "A new label !"; links: [];
                    meta:
                     [<key: "Foo"; value: "Bar">,
                      <key: "a meta"; value: "hehehe">];
                    project: "kohai"; sector: "programming";
                    start_date: "2025-03-01T13-00-00";
                    start_date_repr: "Today at 13:00:00">];
                 inserted: null; outdated: []>>
    |}];
  let () =
    step
      ~desc:{|Add meta into index-0-log.|}
      ~should_fail:false
      ~id
      (call_transient_log_add_meta ~index:0 ~key:"location" ~value:"Nantes")
  in
  [%expect
    {|
    [DONE]: <id: 31; jsonrpc: "2.0";
              result:
               <all:
                 [<duration: null; duration_repr: null; index: 0;
                    label: "A first transient log!"; links: [];
                    meta: [<key: "location"; value: "Nantes">]; project: null;
                    sector: "a-new-sector"; start_date: "2025-03-01T12-00-00";
                    start_date_repr: "Today at 12:00:00">,
                  <duration: 1500; duration_repr: "25m"; index: 1;
                    label: "A new label !"; links: [];
                    meta:
                     [<key: "Foo"; value: "Bar">,
                      <key: "a meta"; value: "hehehe">];
                    project: "kohai"; sector: "programming";
                    start_date: "2025-03-01T13-00-00";
                    start_date_repr: "Today at 13:00:00">];
                 inserted: null; outdated: []>>
    |}];
  let () = F.manip_time Datetime.succ_day in
  let () =
    step
      ~desc:{|Store a new log (to be removed).|}
      ~should_fail:false
      ~id
      (call_transient_log_record
         ?date_query:None
         ?project:None
         ~sector:"programming"
         ~label:"TO BE DELETED !!!!!")
  in
  [%expect
    {|
    [DONE]: <id: 32; jsonrpc: "2.0";
              result:
               <all:
                 [<duration: null; duration_repr: null; index: 0;
                    label: "A first transient log!"; links: [];
                    meta: [<key: "location"; value: "Nantes">]; project: null;
                    sector: "a-new-sector"; start_date: "2025-03-01T12-00-00";
                    start_date_repr: "Yesterday at 12:00:00">,
                  <duration: 1500; duration_repr: "25m"; index: 1;
                    label: "A new label !"; links: [];
                    meta:
                     [<key: "Foo"; value: "Bar">,
                      <key: "a meta"; value: "hehehe">];
                    project: "kohai"; sector: "programming";
                    start_date: "2025-03-01T13-00-00";
                    start_date_repr: "Yesterday at 13:00:00">,
                  <duration: null; duration_repr: null; index: 2;
                    label: "TO BE DELETED !!!!!"; links: []; meta: [];
                    project: null; sector: "programming";
                    start_date: "2025-03-02T01-00-00";
                    start_date_repr: "Today at 01:00:00">];
                 inserted:
                  <duration: null; index: -1; label: "TO BE DELETED !!!!!";
                    links: []; meta: []; project: null; sector: "programming";
                    start_date: "2025-03-02T01-00-00">;
                 outdated:
                  [<computed_duration: 780;
                     record:
                      <duration: null; duration_repr: null; index: 0;
                        label: "A first transient log!"; links: [];
                        meta: [<key: "location"; value: "Nantes">];
                        project: null; sector: "a-new-sector";
                        start_date: "2025-03-01T12-00-00";
                        start_date_repr: "Yesterday at 12:00:00">>]>>
    |}];
  let () =
    step
      ~desc:{|Delete the freshly added log.|}
      ~should_fail:false
      ~id
      (call_transient_log_delete ~index:2)
  in
  [%expect
    {|
    [DONE]: <id: 33; jsonrpc: "2.0";
              result:
               <all:
                 [<duration: null; duration_repr: null; index: 0;
                    label: "A first transient log!"; links: [];
                    meta: [<key: "location"; value: "Nantes">]; project: null;
                    sector: "a-new-sector"; start_date: "2025-03-01T12-00-00";
                    start_date_repr: "Yesterday at 12:00:00">,
                  <duration: 1500; duration_repr: "25m"; index: 1;
                    label: "A new label !"; links: [];
                    meta:
                     [<key: "Foo"; value: "Bar">,
                      <key: "a meta"; value: "hehehe">];
                    project: "kohai"; sector: "programming";
                    start_date: "2025-03-01T13-00-00";
                    start_date_repr: "Yesterday at 13:00:00">];
                 inserted: null; outdated: []>>
    |}];
  let () =
    step
      ~desc:{|Add link to index 1.|}
      ~should_fail:false
      ~id
      (call_transient_log_add_link
         ~index:1
         ~key:"homepage"
         ~value:"https://xvw.lol")
  in
  [%expect
    {|
    [DONE]: <id: 34; jsonrpc: "2.0";
              result:
               <all:
                 [<duration: null; duration_repr: null; index: 0;
                    label: "A first transient log!"; links: [];
                    meta: [<key: "location"; value: "Nantes">]; project: null;
                    sector: "a-new-sector"; start_date: "2025-03-01T12-00-00";
                    start_date_repr: "Yesterday at 12:00:00">,
                  <duration: 1500; duration_repr: "25m"; index: 1;
                    label: "A new label !";
                    links: [<key: "homepage"; value: "https://xvw.lol">];
                    meta:
                     [<key: "Foo"; value: "Bar">,
                      <key: "a meta"; value: "hehehe">];
                    project: "kohai"; sector: "programming";
                    start_date: "2025-03-01T13-00-00";
                    start_date_repr: "Yesterday at 13:00:00">];
                 inserted: null; outdated: []>>
    |}];
  let () =
    step
      ~desc:{|Add link other to index 1.|}
      ~should_fail:false
      ~id
      (call_transient_log_add_link
         ~index:1
         ~key:"google"
         ~value:"https://google.com")
  in
  [%expect
    {|
    [DONE]: <id: 35; jsonrpc: "2.0";
              result:
               <all:
                 [<duration: null; duration_repr: null; index: 0;
                    label: "A first transient log!"; links: [];
                    meta: [<key: "location"; value: "Nantes">]; project: null;
                    sector: "a-new-sector"; start_date: "2025-03-01T12-00-00";
                    start_date_repr: "Yesterday at 12:00:00">,
                  <duration: 1500; duration_repr: "25m"; index: 1;
                    label: "A new label !";
                    links:
                     [<key: "google"; value: "https://google.com">,
                      <key: "homepage"; value: "https://xvw.lol">];
                    meta:
                     [<key: "Foo"; value: "Bar">,
                      <key: "a meta"; value: "hehehe">];
                    project: "kohai"; sector: "programming";
                    start_date: "2025-03-01T13-00-00";
                    start_date_repr: "Yesterday at 13:00:00">];
                 inserted: null; outdated: []>>
    |}];
  let () =
    step
      ~desc:{|Update link other to index 1.|}
      ~should_fail:false
      ~id
      (call_transient_log_add_link
         ~index:1
         ~key:"google"
         ~value:"https://www.google.com")
  in
  [%expect
    {|
    [DONE]: <id: 36; jsonrpc: "2.0";
              result:
               <all:
                 [<duration: null; duration_repr: null; index: 0;
                    label: "A first transient log!"; links: [];
                    meta: [<key: "location"; value: "Nantes">]; project: null;
                    sector: "a-new-sector"; start_date: "2025-03-01T12-00-00";
                    start_date_repr: "Yesterday at 12:00:00">,
                  <duration: 1500; duration_repr: "25m"; index: 1;
                    label: "A new label !";
                    links:
                     [<key: "google"; value: "https://www.google.com">,
                      <key: "homepage"; value: "https://xvw.lol">];
                    meta:
                     [<key: "Foo"; value: "Bar">,
                      <key: "a meta"; value: "hehehe">];
                    project: "kohai"; sector: "programming";
                    start_date: "2025-03-01T13-00-00";
                    start_date_repr: "Yesterday at 13:00:00">];
                 inserted: null; outdated: []>>
    |}];
  let () =
    step
      ~desc:{|Remove link to index 1.|}
      ~should_fail:false
      ~id
      (call_transient_log_remove_link ~index:1 ~key:"google")
  in
  [%expect
    {|
    [DONE]: <id: 37; jsonrpc: "2.0";
              result:
               <all:
                 [<duration: null; duration_repr: null; index: 0;
                    label: "A first transient log!"; links: [];
                    meta: [<key: "location"; value: "Nantes">]; project: null;
                    sector: "a-new-sector"; start_date: "2025-03-01T12-00-00";
                    start_date_repr: "Yesterday at 12:00:00">,
                  <duration: 1500; duration_repr: "25m"; index: 1;
                    label: "A new label !";
                    links: [<key: "homepage"; value: "https://xvw.lol">];
                    meta:
                     [<key: "Foo"; value: "Bar">,
                      <key: "a meta"; value: "hehehe">];
                    project: "kohai"; sector: "programming";
                    start_date: "2025-03-01T13-00-00";
                    start_date_repr: "Yesterday at 13:00:00">];
                 inserted: null; outdated: []>>
    |}];
  let () =
    step
      ~desc:{|Remove meta index 1.|}
      ~should_fail:false
      ~id
      (call_transient_log_remove_meta ~index:1 ~key:"Foo")
  in
  [%expect
    {|
    [DONE]: <id: 38; jsonrpc: "2.0";
              result:
               <all:
                 [<duration: null; duration_repr: null; index: 0;
                    label: "A first transient log!"; links: [];
                    meta: [<key: "location"; value: "Nantes">]; project: null;
                    sector: "a-new-sector"; start_date: "2025-03-01T12-00-00";
                    start_date_repr: "Yesterday at 12:00:00">,
                  <duration: 1500; duration_repr: "25m"; index: 1;
                    label: "A new label !";
                    links: [<key: "homepage"; value: "https://xvw.lol">];
                    meta: [<key: "a meta"; value: "hehehe">]; project: "kohai";
                    sector: "programming"; start_date: "2025-03-01T13-00-00";
                    start_date_repr: "Yesterday at 13:00:00">];
                 inserted: null; outdated: []>>
     |}];
  let () =
    step
      ~desc:{|Fetch the state that should be empty|}
      ~should_fail:false
      ~id
      call_state_get
  in
  [%expect
    {|
    [DONE]: <id: 39; jsonrpc: "2.0";
              result:
               <big_bang: null; duration: 0; end_of_world: null;
                 number_of_logs: 0>>
    |}];
  let () =
    step
      ~desc:{|Promote the log index-1.|}
      ~should_fail:false
      ~id
      (call_transient_log_promote ~index:1)
  in
  [%expect
    {|
    [DONE]: <id: 40; jsonrpc: "2.0";
              result:
               <all:
                 [<duration: null; duration_repr: null; index: 0;
                    label: "A first transient log!"; links: [];
                    meta: [<key: "location"; value: "Nantes">]; project: null;
                    sector: "a-new-sector"; start_date: "2025-03-01T12-00-00";
                    start_date_repr: "Yesterday at 12:00:00">];
                 inserted: null; outdated: []>>
     |}];
  let () =
    step
      ~desc:{|Fetch the state that should be filled|}
      ~should_fail:false
      ~id
      call_state_get
  in
  [%expect
    {|
    [DONE]: <id: 41; jsonrpc: "2.0";
              result:
               <big_bang: "2025-03-01T13-00-00"; duration: 1500;
                 end_of_world: "2025-03-01T13-25-00"; number_of_logs: 1>>
     |}];
  let () =
    step
      ~desc:{|Fetch the state for a regular sector|}
      ~should_fail:false
      ~id
      (call_state_get_for_sector ~sector:"programming")
  in
  [%expect
    {|
    [DONE]: <id: 42; jsonrpc: "2.0";
              result:
               <big_bang: "2025-03-01T13-00-00"; duration: 1500;
                 end_of_world: "2025-03-01T13-25-00"; number_of_logs: 1>>
     |}];
  let () =
    step
      ~desc:{|Fetch the state for a regular project|}
      ~should_fail:false
      ~id
      (call_state_get_for_project ~project:"kohai")
  in
  [%expect
    {|
    [DONE]: <id: 43; jsonrpc: "2.0";
              result:
               <big_bang: "2025-03-01T13-00-00"; duration: 1500;
                 end_of_world: "2025-03-01T13-25-00"; number_of_logs: 1>>
     |}];
  let () =
    step
      ~desc:{|Fetch the state for an inexesisting regular project|}
      ~should_fail:false
      ~id
      (call_state_get_for_project ~project:"i-do-not-exists")
  in
  [%expect
    {|
    [DONE]: <id: 44; jsonrpc: "2.0";
              result:
               <big_bang: null; duration: 0; end_of_world: null;
                 number_of_logs: 0>>
    |}];
  let () =
    step
      ~desc:{|Promote the log index-0 (should fail).|}
      ~should_fail:true
      ~id
      (call_transient_log_promote ~index:0)
  in
  [%expect
    {|
    [DONE]: <error:
              <code: -32002; data: "transient log 0 does not exists";
                input:
                 "{"jsonrpc": "2.0", "method": "kohai/transient-log/action", "id": 45, "params": {"ctor":"promote","value":{"index":0}}}";
                message: "Server error">;
              id: 45; jsonrpc: "2.0">
    |}];
  let () =
    step
      ~desc:{|Get the list of sector counter should be increased.|}
      ~should_fail:false
      ~id
      call_sector_list
  in
  [%expect
    {|
    [DONE]: <id: 46; jsonrpc: "2.0";
              result:
               [<counter: 0; description: null; name: "a-new-sector">,
                <counter: 1; description: "Category related to programming";
                  name: "programming">,
                <counter: 0; description: "A description"; name: "visual">]>
    |}];
  let () =
    step
      ~desc:{|Try to delete a sector (that can't be deleted).|}
      ~should_fail:false
      ~id
      (call_sector_delete ~name:"programming")
  in
  [%expect
    {|
    [DONE]: <id: 47; jsonrpc: "2.0";
              result:
               [<counter: 0; description: null; name: "a-new-sector">,
                <counter: 1; description: "Category related to programming";
                  name: "programming">,
                <counter: 0; description: "A description"; name: "visual">]>
    |}];
  let () =
    step
      ~desc:{|Get the list of project counter should be increased.|}
      ~should_fail:false
      ~id
      call_project_list
  in
  [%expect
    {|
    [DONE]: <id: 48; jsonrpc: "2.0";
              result:
               [<counter: 0;
                  description: "My personnal website built with OCaml";
                  name: "capsule">,
                <counter: 1; description: "An opinionated timetracker";
                  name: "kohai">]>
    |}];
  let () =
    step
      ~desc:{|Try to delete a project (that can't be deleted).|}
      ~should_fail:false
      ~id
      (call_project_delete ~name:"kohai")
  in
  [%expect
    {|
    [DONE]: <id: 49; jsonrpc: "2.0";
              result:
               [<counter: 0;
                  description: "My personnal website built with OCaml";
                  name: "capsule">,
                <counter: 1; description: "An opinionated timetracker";
                  name: "kohai">]>
     |}];
  let () =
    step
      ~desc:{|Get the list of last logs.|}
      ~should_fail:false
      ~id
      call_log_last
  in
  [%expect
    {|
    [DONE]: <id: 50; jsonrpc: "2.0";
              result:
               [<duration: 1500; duration_repr: "25m";
                  id: "6e2b10c7-a6f1-5b60-8166-7ecf4906d2f1";
                  label: "A new label !";
                  links: [<key: "homepage"; value: "https://xvw.lol">];
                  meta: [<key: "a meta"; value: "hehehe">]; project: "kohai";
                  sector: "programming"; start_date: "2025-03-01T13-00-00";
                  start_date_repr: "Yesterday at 13:00:00">]>
     |}];
  let () =
    step
      ~desc:{|Close the last log.|}
      ~should_fail:false
      ~id
      (call_transient_log_stop_recording ~index:0 ?duration:None)
  in
  [%expect
    {|
    [DONE]: <id: 51; jsonrpc: "2.0";
              result:
               <all:
                 [<duration: 46800; duration_repr: "13h"; index: 0;
                    label: "A first transient log!"; links: [];
                    meta: [<key: "location"; value: "Nantes">]; project: null;
                    sector: "a-new-sector"; start_date: "2025-03-01T12-00-00";
                    start_date_repr: "Yesterday at 12:00:00">];
                 inserted: null; outdated: []>>
    |}];
  let () =
    step
      ~desc:{|Close the last log.|}
      ~should_fail:false
      ~id
      (call_transient_log_promote ~index:0)
  in
  [%expect
    {|
    [DONE]: <id: 52; jsonrpc: "2.0";
              result: <all: []; inserted: null; outdated: []>>
     |}];
  let () =
    step
      ~desc:{|Get the list of last logs.|}
      ~should_fail:false
      ~id
      call_log_last
  in
  [%expect
    {|
    [DONE]: <id: 53; jsonrpc: "2.0";
              result:
               [<duration: 1500; duration_repr: "25m";
                  id: "6e2b10c7-a6f1-5b60-8166-7ecf4906d2f1";
                  label: "A new label !";
                  links: [<key: "homepage"; value: "https://xvw.lol">];
                  meta: [<key: "a meta"; value: "hehehe">]; project: "kohai";
                  sector: "programming"; start_date: "2025-03-01T13-00-00";
                  start_date_repr: "Yesterday at 13:00:00">,
                <duration: 46800; duration_repr: "13h";
                  id: "fee72312-846f-5b0a-9ccf-317722f1eba6";
                  label: "A first transient log!"; links: [];
                  meta: [<key: "location"; value: "Nantes">]; project: null;
                  sector: "a-new-sector"; start_date: "2025-03-01T12-00-00";
                  start_date_repr: "Yesterday at 12:00:00">]>
     |}];
  let () =
    step
      ~desc:{|Get the list of last logs for project kohai.|}
      ~should_fail:false
      ~id
      (call_log_last_for_project ~project:"programming")
  in
  [%expect {| [DONE]: <id: 54; jsonrpc: "2.0"; result: []> |}];
  let () =
    step
      ~desc:{|Get the list of last logs for project kohai.|}
      ~should_fail:false
      ~id
      (call_log_last_for_project ~project:"kohai")
  in
  [%expect
    {|
    [DONE]: <id: 55; jsonrpc: "2.0";
              result:
               [<duration: 1500; duration_repr: "25m";
                  id: "6e2b10c7-a6f1-5b60-8166-7ecf4906d2f1";
                  label: "A new label !";
                  links: [<key: "homepage"; value: "https://xvw.lol">];
                  meta: [<key: "a meta"; value: "hehehe">]; project: "kohai";
                  sector: "programming"; start_date: "2025-03-01T13-00-00";
                  start_date_repr: "Yesterday at 13:00:00">]>
     |}];
  let () =
    step
      ~desc:{|Get the list of last logs for sector programming.|}
      ~should_fail:false
      ~id
      (call_log_last_for_sector ~sector:"programming")
  in
  [%expect
    {|
    [DONE]: <id: 56; jsonrpc: "2.0";
              result:
               [<duration: 1500; duration_repr: "25m";
                  id: "6e2b10c7-a6f1-5b60-8166-7ecf4906d2f1";
                  label: "A new label !";
                  links: [<key: "homepage"; value: "https://xvw.lol">];
                  meta: [<key: "a meta"; value: "hehehe">]; project: "kohai";
                  sector: "programming"; start_date: "2025-03-01T13-00-00";
                  start_date_repr: "Yesterday at 13:00:00">]>
     |}];
  let () =
    step
      ~desc:{|Get the list of last logs for sector a-new-sector.|}
      ~should_fail:false
      ~id
      (call_log_last_for_sector ~sector:"a-new-sector")
  in
  [%expect
    {|
    [DONE]: <id: 57; jsonrpc: "2.0";
              result:
               [<duration: 46800; duration_repr: "13h";
                  id: "fee72312-846f-5b0a-9ccf-317722f1eba6";
                  label: "A first transient log!"; links: [];
                  meta: [<key: "location"; value: "Nantes">]; project: null;
                  sector: "a-new-sector"; start_date: "2025-03-01T12-00-00";
                  start_date_repr: "Yesterday at 12:00:00">]>
     |}];
  let () =
    step
      ~desc:{|Get the transient log list.|}
      ~should_fail:false
      ~id
      call_transient_log_list
  in
  [%expect {| [DONE]: <id: 58; jsonrpc: "2.0"; result: []> |}];
  let () =
    step
      ~desc:
        {|Retreive state
       (to see if it was removed).|}
      ~should_fail:false
      ~id
      call_state_get
  in
  [%expect
    {|
    [DONE]: <id: 59; jsonrpc: "2.0";
              result:
               <big_bang: "2025-03-01T12-00-00"; duration: 48300;
                 end_of_world: "2025-03-02T01-00-00"; number_of_logs: 2>>
    |}];
  let () =
    step
      ~desc:{|Unpromote promoted log.|}
      ~should_fail:false
      ~id
      (call_log_unpromote ~uuid:"fee72312-846f-5b0a-9ccf-317722f1eba6")
  in
  [%expect
    {|
    [DONE]: <id: 60; jsonrpc: "2.0";
              result:
               [<duration: 46800; duration_repr: "13h"; index: 0;
                  label: "A first transient log!"; links: [];
                  meta: [<key: "location"; value: "Nantes">]; project: null;
                  sector: "a-new-sector"; start_date: "2025-03-01T12-00-00";
                  start_date_repr: "Yesterday at 12:00:00">]>
    |}];
  let () =
    step
      ~desc:
        {|Get the list of last logs
       (to see if it was removed).|}
      ~should_fail:false
      ~id
      call_log_last
  in
  [%expect
    {|
    [DONE]: <id: 61; jsonrpc: "2.0";
              result:
               [<duration: 1500; duration_repr: "25m";
                  id: "6e2b10c7-a6f1-5b60-8166-7ecf4906d2f1";
                  label: "A new label !";
                  links: [<key: "homepage"; value: "https://xvw.lol">];
                  meta: [<key: "a meta"; value: "hehehe">]; project: "kohai";
                  sector: "programming"; start_date: "2025-03-01T13-00-00";
                  start_date_repr: "Yesterday at 13:00:00">]>
    |}];
  let () =
    step
      ~desc:
        {|Get the list of last logs for sector a-new-sector
       (to see if it was removed).|}
      ~should_fail:false
      ~id
      (call_log_last_for_sector ~sector:"a-new-sector")
  in
  [%expect {| [DONE]: <id: 62; jsonrpc: "2.0"; result: []> |}];
  let () =
    step
      ~desc:
        {|Retreive state
       (to see if it was removed).|}
      ~should_fail:false
      ~id
      call_state_get
  in
  [%expect
    {|
    [DONE]: <id: 63; jsonrpc: "2.0";
              result:
               <big_bang: "2025-03-01T12-00-00"; duration: 1500;
                 end_of_world: "2025-03-02T01-00-00"; number_of_logs: 1>>
    |}];
  let () =
    step
      ~desc:
        {|Retreive state for a sector
       (to see if it was removed).|}
      ~should_fail:false
      ~id
      (call_state_get_for_sector ~sector:"a-new-sector")
  in
  [%expect
    {|
    [DONE]: <id: 64; jsonrpc: "2.0";
              result:
               <big_bang: "2025-03-01T12-00-00"; duration: 0;
                 end_of_world: "2025-03-02T01-00-00"; number_of_logs: 0>>
    |}];
  print_endline "[DONE]";
  [%expect {| [DONE] |}]
;;
