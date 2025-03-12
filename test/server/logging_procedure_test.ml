open Kohai_core
open Util

module F = Virtfs.Make (struct
    let fs = Virtfs.(from_list [ dir ".kohai" [] ])

    let now =
      Datetime.(make ~time:(10, 0, 0) ~year:2025 ~month:Mar ~day:1 ())
      |> Result.get_ok
    ;;
  end)

module H = Eff.Handler (F)

let id = ref 0
let exec = step (module H) ~id

let%expect_test
    {|
     As no supervised directory has been retrieving the
     supervised directory should return `null`. |}
  =
  let action = call_supervise_get
  and should_fail = false in
  exec ~should_fail action;
  [%expect {| [DONE]: <id: 0; jsonrpc: "2.0"; result: null> |}]
;;

let%expect_test
    {|
     Assigns a non-existing supervision directory.
     So the request should fail. |}
  =
  let action = call_supervise ~path:"/.logging"
  and should_fail = true in
  exec ~should_fail action;
  [%expect
    {|
    [DONE]: <error:
              <body:
                "{"jsonrpc": "2.0", "method": "kohai/supervision/set", "id": 1, "params": "/.logging"}";
                code: -32001; data: "The given directory does not exists";
                message: "Server error">;
              id: 1; jsonrpc: "2.0">
    |}]
;;

let%expect_test
    {|
       As no supervised directory has been submitted,
       retrieving the supervised directory should return `null`. |}
  =
  let action = call_supervise_get
  and should_fail = false in
  exec ~should_fail action;
  [%expect {| [DONE]: <id: 2; jsonrpc: "2.0"; result: null> |}]
;;

let%expect_test
    {| Fetch the state that should fail since there is no supervision. |}
  =
  let action = call_state_get
  and should_fail = true in
  exec ~should_fail action;
  [%expect
    {|
    [DONE]: <error:
              <body: "{"jsonrpc": "2.0", "method": "kohai/state/get", "id": 3}";
                code: -32000;
                data: "No supervised directory for the current session";
                message: "Server error">;
              id: 3; jsonrpc: "2.0">
    |}]
;;

let%expect_test {| Assigns an existing supervision directory. |} =
  let action = call_supervise ~path:"/.kohai"
  and should_fail = false in
  exec ~should_fail action;
  [%expect {| [DONE]: <id: 4; jsonrpc: "2.0"; result: "/.kohai"> |}]
;;

let%expect_test {| Get the list of sector (should be empty). |} =
  let action = call_sector_list
  and should_fail = false in
  exec ~should_fail action;
  [%expect {| [DONE]: <id: 5; jsonrpc: "2.0"; result: []> |}]
;;

let%expect_test {| Get the list of projects (should be empty). |} =
  let action = call_project_list
  and should_fail = false in
  exec ~should_fail action;
  [%expect {| [DONE]: <id: 6; jsonrpc: "2.0"; result: []> |}]
;;

let%expect_test {| Save a sector. |} =
  let action =
    call_sector_save ~name:"programming" ~desc:"Category related to programming"
  and should_fail = false in
  exec ~should_fail action;
  [%expect
    {|
    [DONE]: <id: 7; jsonrpc: "2.0";
              result:
               [<counter: 0; description: "Category related to programming";
                  name: "programming">]>
    |}]
;;

let%expect_test {| Save an other sector. |} =
  let action = call_sector_save ~name:"visual" ?desc:None
  and should_fail = false in
  exec ~should_fail action;
  [%expect
    {|
    [DONE]: <id: 8; jsonrpc: "2.0";
              result:
               [<counter: 0; description: "Category related to programming";
                  name: "programming">,
                <counter: 0; description: null; name: "visual">]>
    |}]
;;

let%expect_test {| Patch an existing sector without updates. |} =
  let action = call_sector_save ~name:"programming" ?desc:None
  and should_fail = false in
  exec ~should_fail action;
  [%expect
    {|
    [DONE]: <id: 9; jsonrpc: "2.0";
              result:
               [<counter: 0; description: "Category related to programming";
                  name: "programming">,
                <counter: 0; description: null; name: "visual">]>
    |}]
;;

let%expect_test {| Patch an existing sector with updates. |} =
  let action = call_sector_save ~name:"visual" ~desc:"A description"
  and should_fail = false in
  exec ~should_fail action;
  [%expect
    {|
    [DONE]: <id: 10; jsonrpc: "2.0";
              result:
               [<counter: 0; description: "Category related to programming";
                  name: "programming">,
                <counter: 0; description: "A description"; name: "visual">]>
    |}]
;;

let%expect_test {| Get the list of sector (should be filled with 2 entries). |} =
  let action = call_sector_list
  and should_fail = false in
  exec ~should_fail action;
  [%expect
    {|
    [DONE]: <id: 11; jsonrpc: "2.0";
              result:
               [<counter: 0; description: "Category related to programming";
                  name: "programming">,
                <counter: 0; description: "A description"; name: "visual">]>
    |}]
;;

let%expect_test {| Save a project. |} =
  let action =
    call_project_save ~name:"kohai" ~desc:"An opinionated timetracker"
  and should_fail = false in
  exec ~should_fail action;
  [%expect
    {|
    [DONE]: <id: 12; jsonrpc: "2.0";
              result:
               [<counter: 0; description: "An opinionated timetracker";
                  name: "kohai">]>
    |}]
;;

let%expect_test {| Save an other project. |} =
  let action = call_project_save ~name:"capsule" ?desc:None
  and should_fail = false in
  exec ~should_fail action;
  [%expect
    {|
    [DONE]: <id: 13; jsonrpc: "2.0";
              result:
               [<counter: 0; description: null; name: "capsule">,
                <counter: 0; description: "An opinionated timetracker";
                  name: "kohai">]>
    |}]
;;

let%expect_test {| Patch project without updates. |} =
  let action = call_project_save ~name:"kohai" ?desc:None
  and should_fail = false in
  exec ~should_fail action;
  [%expect
    {|
    [DONE]: <id: 14; jsonrpc: "2.0";
              result:
               [<counter: 0; description: null; name: "capsule">,
                <counter: 0; description: "An opinionated timetracker";
                  name: "kohai">]>
    |}]
;;

let%expect_test {| Patch project with updates. |} =
  let action =
    call_project_save
      ~name:"capsule"
      ~desc:"My personnal website built with OCaml"
  and should_fail = false in
  exec ~should_fail action;
  [%expect
    {|
    [DONE]: <id: 15; jsonrpc: "2.0";
              result:
               [<counter: 0;
                  description: "My personnal website built with OCaml";
                  name: "capsule">,
                <counter: 0; description: "An opinionated timetracker";
                  name: "kohai">]>
    |}]
;;

let%expect_test {| Get the list of project (should be filled with 2 entries). |}
  =
  let action = call_project_list
  and should_fail = false in
  exec ~should_fail action;
  [%expect
    {|
    [DONE]: <id: 16; jsonrpc: "2.0";
              result:
               [<counter: 0;
                  description: "My personnal website built with OCaml";
                  name: "capsule">,
                <counter: 0; description: "An opinionated timetracker";
                  name: "kohai">]>
    |}]
;;

let%expect_test {| Save a project (in order to be deleted). |} =
  let action = call_project_save ~name:"preface" ~desc:"A library"
  and should_fail = false in
  exec ~should_fail action;
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
    |}]
;;

let%expect_test {| Delete the project. |} =
  let action = call_project_delete ~name:"preface"
  and should_fail = false in
  exec ~should_fail action;
  [%expect
    {|
    [DONE]: <id: 18; jsonrpc: "2.0";
              result:
               [<counter: 0;
                  description: "My personnal website built with OCaml";
                  name: "capsule">,
                <counter: 0; description: "An opinionated timetracker";
                  name: "kohai">]>
    |}]
;;

let%expect_test {| Save a sector (in order to be deleted). |} =
  let action = call_sector_save ~name:"painting" ~desc:"desc"
  and should_fail = false in
  exec ~should_fail action;
  [%expect
    {|
    [DONE]: <id: 19; jsonrpc: "2.0";
              result:
               [<counter: 0; description: "desc"; name: "painting">,
                <counter: 0; description: "Category related to programming";
                  name: "programming">,
                <counter: 0; description: "A description"; name: "visual">]>
    |}]
;;

let%expect_test {| Delete the sector. |} =
  let action = call_sector_delete ~name:"painting"
  and should_fail = false in
  exec ~should_fail action;
  [%expect
    {|
    [DONE]: <id: 20; jsonrpc: "2.0";
              result:
               [<counter: 0; description: "Category related to programming";
                  name: "programming">,
                <counter: 0; description: "A description"; name: "visual">]>
    |}]
;;

let%expect_test {| Get the transient log list (should be empty). |} =
  let action = call_transient_log_list
  and should_fail = false in
  exec ~should_fail action;
  [%expect {| [DONE]: <id: 21; jsonrpc: "2.0"; result: []> |}]
;;

let%expect_test
    {|
       Store a transient log with an existing project and
       an existing sector. |}
  =
  let action =
    call_transient_log_record
      ?date_query:None
      ~project:"kohai"
      ~sector:"programming"
      ~label:"A first transient log!"
  and should_fail = false in
  exec ~should_fail action;
  [%expect
    {|
    [DONE]: <id: 22; jsonrpc: "2.0";
              result:
               <all:
                 [<duration: null; duration_repr: null; index: 0;
                    label: "A first transient log!"; links: []; meta: [];
                    project: "kohai"; sector: "programming";
                    start_date: "2025-03-01T10-00-00";
                    start_date_repr: "Today at 10:00:00">];
                 inserted:
                  <duration: null; index: -1; label: "A first transient log!";
                    links: []; meta: []; project: "kohai"; sector: "programming";
                    start_date: "2025-03-01T10-00-00">;
                 outdated: []>>
    |}]
;;

let%expect_test
    {|
       Get the transient log list (should be filled with
       one element). |}
  =
  let action = call_transient_log_list
  and should_fail = false in
  exec ~should_fail action;
  [%expect
    {|
    [DONE]: <id: 23; jsonrpc: "2.0";
              result:
               [<duration: null; duration_repr: null; index: 0;
                  label: "A first transient log!"; links: []; meta: [];
                  project: "kohai"; sector: "programming";
                  start_date: "2025-03-01T10-00-00";
                  start_date_repr: "Today at 10:00:00">]>
    |}]
;;

let%expect_test
    {|
       Store an other transient log without project and an
       non-existing sector. |}
  =
  let action =
    call_transient_log_record
      ?date_query:None
      ?project:None
      ~sector:"a-new-sector"
      ~label:"A first transient log!"
  and should_fail = false in
  let () = F.manip_time Datetime.succ_hour in
  exec ~should_fail action;
  [%expect
    {|
    [DONE]: <id: 24; jsonrpc: "2.0";
              result:
               <all:
                 [<duration: null; duration_repr: null; index: 0;
                    label: "A first transient log!"; links: []; meta: [];
                    project: "kohai"; sector: "programming";
                    start_date: "2025-03-01T10-00-00";
                    start_date_repr: "Today at 10:00:00">,
                  <duration: null; duration_repr: null; index: 1;
                    label: "A first transient log!"; links: []; meta: [];
                    project: null; sector: "a-new-sector";
                    start_date: "2025-03-01T11-00-00";
                    start_date_repr: "Today at 11:00:00">];
                 inserted:
                  <duration: null; index: -1; label: "A first transient log!";
                    links: []; meta: []; project: null; sector: "a-new-sector";
                    start_date: "2025-03-01T11-00-00">;
                 outdated:
                  [<computed_duration: 60;
                     record:
                      <duration: null; duration_repr: null; index: 0;
                        label: "A first transient log!"; links: []; meta: [];
                        project: "kohai"; sector: "programming";
                        start_date: "2025-03-01T10-00-00";
                        start_date_repr: "Today at 10:00:00">>]>>
    |}]
;;

let%expect_test {| Get the list of sector (should be filled with 3 entries). |} =
  let action = call_sector_list
  and should_fail = false in
  exec ~should_fail action;
  [%expect
    {|
    [DONE]: <id: 25; jsonrpc: "2.0";
              result:
               [<counter: 0; description: null; name: "a-new-sector">,
                <counter: 0; description: "Category related to programming";
                  name: "programming">,
                <counter: 0; description: "A description"; name: "visual">]>
    |}]
;;

let%expect_test {| Close a the log (indexed-0) with the default duration. |} =
  let action = call_transient_log_stop_recording ~index:0 ?duration:None
  and should_fail = false in
  let () = F.manip_time Datetime.succ_hour in
  exec ~should_fail action;
  [%expect
    {|
    [DONE]: <id: 26; jsonrpc: "2.0";
              result:
               <all:
                 [<duration: 7200; duration_repr: "2h"; index: 0;
                    label: "A first transient log!"; links: []; meta: [];
                    project: "kohai"; sector: "programming";
                    start_date: "2025-03-01T10-00-00";
                    start_date_repr: "Today at 10:00:00">,
                  <duration: null; duration_repr: null; index: 1;
                    label: "A first transient log!"; links: []; meta: [];
                    project: null; sector: "a-new-sector";
                    start_date: "2025-03-01T11-00-00";
                    start_date_repr: "Today at 11:00:00">];
                 inserted: null; outdated: []>>
    |}]
;;

let%expect_test {| Rewrite the 0-indexed log. |} =
  let action =
    call_transient_log_rewrite
      ~index:0
      ~sector:"programming"
      ?date_query:None
      ~project:"kohai"
      ~label:"A new label !"
  and should_fail = false in
  exec ~should_fail action;
  [%expect
    {|
    [DONE]: <id: 27; jsonrpc: "2.0";
              result:
               <all:
                 [<duration: null; duration_repr: null; index: 0;
                    label: "A first transient log!"; links: []; meta: [];
                    project: null; sector: "a-new-sector";
                    start_date: "2025-03-01T11-00-00";
                    start_date_repr: "Today at 11:00:00">,
                  <duration: null; duration_repr: null; index: 1;
                    label: "A new label !"; links: []; meta: [];
                    project: "kohai"; sector: "programming";
                    start_date: "2025-03-01T12-00-00";
                    start_date_repr: "Today at 12:00:00">];
                 inserted: null; outdated: []>>
    |}]
;;

let%expect_test {| Close a the log (indexed-1) with a given duration. |} =
  let () = F.manip_time Datetime.succ_hour in
  let action = call_transient_log_stop_recording ~index:1 ~duration:25
  and should_fail = false in
  exec ~should_fail action;
  [%expect
    {|
    [DONE]: <id: 28; jsonrpc: "2.0";
              result:
               <all:
                 [<duration: null; duration_repr: null; index: 0;
                    label: "A first transient log!"; links: []; meta: [];
                    project: null; sector: "a-new-sector";
                    start_date: "2025-03-01T11-00-00";
                    start_date_repr: "Today at 11:00:00">,
                  <duration: 1500; duration_repr: "25m"; index: 1;
                    label: "A new label !"; links: []; meta: [];
                    project: "kohai"; sector: "programming";
                    start_date: "2025-03-01T12-00-00";
                    start_date_repr: "Today at 12:00:00">];
                 inserted: null; outdated: []>>
    |}]
;;

let%expect_test {| Add meta into index-1-log. |} =
  let action = call_transient_log_add_meta ~index:1 ~key:"Foo" ~value:"Bar"
  and should_fail = false in
  exec ~should_fail action;
  [%expect
    {|
    [DONE]: <id: 29; jsonrpc: "2.0";
              result:
               <all:
                 [<duration: null; duration_repr: null; index: 0;
                    label: "A first transient log!"; links: []; meta: [];
                    project: null; sector: "a-new-sector";
                    start_date: "2025-03-01T11-00-00";
                    start_date_repr: "Today at 11:00:00">,
                  <duration: 1500; duration_repr: "25m"; index: 1;
                    label: "A new label !"; links: [];
                    meta: [<key: "Foo"; value: "Bar">]; project: "kohai";
                    sector: "programming"; start_date: "2025-03-01T12-00-00";
                    start_date_repr: "Today at 12:00:00">];
                 inserted: null; outdated: []>>
    |}]
;;

let%expect_test {| Add meta into index-1-log. |} =
  let action =
    call_transient_log_add_meta ~index:1 ~key:"a meta" ~value:"hehehe"
  and should_fail = false in
  exec ~should_fail action;
  [%expect
    {|
    [DONE]: <id: 30; jsonrpc: "2.0";
              result:
               <all:
                 [<duration: null; duration_repr: null; index: 0;
                    label: "A first transient log!"; links: []; meta: [];
                    project: null; sector: "a-new-sector";
                    start_date: "2025-03-01T11-00-00";
                    start_date_repr: "Today at 11:00:00">,
                  <duration: 1500; duration_repr: "25m"; index: 1;
                    label: "A new label !"; links: [];
                    meta:
                     [<key: "Foo"; value: "Bar">,
                      <key: "a meta"; value: "hehehe">];
                    project: "kohai"; sector: "programming";
                    start_date: "2025-03-01T12-00-00";
                    start_date_repr: "Today at 12:00:00">];
                 inserted: null; outdated: []>>
    |}]
;;

let%expect_test {| Add meta into index-0-log. |} =
  let action =
    call_transient_log_add_meta ~index:0 ~key:"location" ~value:"Nantes"
  and should_fail = false in
  exec ~should_fail action;
  [%expect
    {|
    [DONE]: <id: 31; jsonrpc: "2.0";
              result:
               <all:
                 [<duration: null; duration_repr: null; index: 0;
                    label: "A first transient log!"; links: [];
                    meta: [<key: "location"; value: "Nantes">]; project: null;
                    sector: "a-new-sector"; start_date: "2025-03-01T11-00-00";
                    start_date_repr: "Today at 11:00:00">,
                  <duration: 1500; duration_repr: "25m"; index: 1;
                    label: "A new label !"; links: [];
                    meta:
                     [<key: "Foo"; value: "Bar">,
                      <key: "a meta"; value: "hehehe">];
                    project: "kohai"; sector: "programming";
                    start_date: "2025-03-01T12-00-00";
                    start_date_repr: "Today at 12:00:00">];
                 inserted: null; outdated: []>>
    |}]
;;

let%expect_test {| Store a new log (to be removed). |} =
  let () = F.manip_time Datetime.succ_day in
  let action =
    call_transient_log_record
      ?date_query:None
      ?project:None
      ~sector:"programming"
      ~label:"TO BE DELETED !!!!!"
  and should_fail = false in
  exec ~should_fail action;
  [%expect
    {|
    [DONE]: <id: 32; jsonrpc: "2.0";
              result:
               <all:
                 [<duration: null; duration_repr: null; index: 0;
                    label: "A first transient log!"; links: [];
                    meta: [<key: "location"; value: "Nantes">]; project: null;
                    sector: "a-new-sector"; start_date: "2025-03-01T11-00-00";
                    start_date_repr: "Yesterday at 11:00:00">,
                  <duration: 1500; duration_repr: "25m"; index: 1;
                    label: "A new label !"; links: [];
                    meta:
                     [<key: "Foo"; value: "Bar">,
                      <key: "a meta"; value: "hehehe">];
                    project: "kohai"; sector: "programming";
                    start_date: "2025-03-01T12-00-00";
                    start_date_repr: "Yesterday at 12:00:00">,
                  <duration: null; duration_repr: null; index: 2;
                    label: "TO BE DELETED !!!!!"; links: []; meta: [];
                    project: null; sector: "programming";
                    start_date: "2025-03-02T00-00-00";
                    start_date_repr: "Today at 00:00:00">];
                 inserted:
                  <duration: null; index: -1; label: "TO BE DELETED !!!!!";
                    links: []; meta: []; project: null; sector: "programming";
                    start_date: "2025-03-02T00-00-00">;
                 outdated:
                  [<computed_duration: 780;
                     record:
                      <duration: null; duration_repr: null; index: 0;
                        label: "A first transient log!"; links: [];
                        meta: [<key: "location"; value: "Nantes">];
                        project: null; sector: "a-new-sector";
                        start_date: "2025-03-01T11-00-00";
                        start_date_repr: "Yesterday at 11:00:00">>]>>
    |}]
;;

let%expect_test {| Delete the freshly added log. |} =
  let action = call_transient_log_delete ~index:2
  and should_fail = false in
  exec ~should_fail action;
  [%expect
    {|
    [DONE]: <id: 33; jsonrpc: "2.0";
              result:
               <all:
                 [<duration: null; duration_repr: null; index: 0;
                    label: "A first transient log!"; links: [];
                    meta: [<key: "location"; value: "Nantes">]; project: null;
                    sector: "a-new-sector"; start_date: "2025-03-01T11-00-00";
                    start_date_repr: "Yesterday at 11:00:00">,
                  <duration: 1500; duration_repr: "25m"; index: 1;
                    label: "A new label !"; links: [];
                    meta:
                     [<key: "Foo"; value: "Bar">,
                      <key: "a meta"; value: "hehehe">];
                    project: "kohai"; sector: "programming";
                    start_date: "2025-03-01T12-00-00";
                    start_date_repr: "Yesterday at 12:00:00">];
                 inserted: null; outdated: []>>
    |}]
;;

let%expect_test {| Add link to index 1. |} =
  let action =
    call_transient_log_add_link
      ~index:1
      ~key:"homepage"
      ~value:"https://xvw.lol"
  and should_fail = false in
  exec ~should_fail action;
  [%expect
    {|
    [DONE]: <id: 34; jsonrpc: "2.0";
              result:
               <all:
                 [<duration: null; duration_repr: null; index: 0;
                    label: "A first transient log!"; links: [];
                    meta: [<key: "location"; value: "Nantes">]; project: null;
                    sector: "a-new-sector"; start_date: "2025-03-01T11-00-00";
                    start_date_repr: "Yesterday at 11:00:00">,
                  <duration: 1500; duration_repr: "25m"; index: 1;
                    label: "A new label !";
                    links: [<key: "homepage"; value: "https://xvw.lol">];
                    meta:
                     [<key: "Foo"; value: "Bar">,
                      <key: "a meta"; value: "hehehe">];
                    project: "kohai"; sector: "programming";
                    start_date: "2025-03-01T12-00-00";
                    start_date_repr: "Yesterday at 12:00:00">];
                 inserted: null; outdated: []>>
    |}]
;;

let%expect_test {| Add other link to index 1. |} =
  let action =
    call_transient_log_add_link
      ~index:1
      ~key:"google"
      ~value:"https://google.com"
  and should_fail = false in
  exec ~should_fail action;
  [%expect
    {|
    [DONE]: <id: 35; jsonrpc: "2.0";
              result:
               <all:
                 [<duration: null; duration_repr: null; index: 0;
                    label: "A first transient log!"; links: [];
                    meta: [<key: "location"; value: "Nantes">]; project: null;
                    sector: "a-new-sector"; start_date: "2025-03-01T11-00-00";
                    start_date_repr: "Yesterday at 11:00:00">,
                  <duration: 1500; duration_repr: "25m"; index: 1;
                    label: "A new label !";
                    links:
                     [<key: "google"; value: "https://google.com">,
                      <key: "homepage"; value: "https://xvw.lol">];
                    meta:
                     [<key: "Foo"; value: "Bar">,
                      <key: "a meta"; value: "hehehe">];
                    project: "kohai"; sector: "programming";
                    start_date: "2025-03-01T12-00-00";
                    start_date_repr: "Yesterday at 12:00:00">];
                 inserted: null; outdated: []>>
    |}]
;;

let%expect_test {| Update link to index 1. |} =
  let action =
    call_transient_log_add_link
      ~index:1
      ~key:"google"
      ~value:"https://www.google.com"
  and should_fail = false in
  exec ~should_fail action;
  [%expect
    {|
    [DONE]: <id: 36; jsonrpc: "2.0";
              result:
               <all:
                 [<duration: null; duration_repr: null; index: 0;
                    label: "A first transient log!"; links: [];
                    meta: [<key: "location"; value: "Nantes">]; project: null;
                    sector: "a-new-sector"; start_date: "2025-03-01T11-00-00";
                    start_date_repr: "Yesterday at 11:00:00">,
                  <duration: 1500; duration_repr: "25m"; index: 1;
                    label: "A new label !";
                    links:
                     [<key: "google"; value: "https://www.google.com">,
                      <key: "homepage"; value: "https://xvw.lol">];
                    meta:
                     [<key: "Foo"; value: "Bar">,
                      <key: "a meta"; value: "hehehe">];
                    project: "kohai"; sector: "programming";
                    start_date: "2025-03-01T12-00-00";
                    start_date_repr: "Yesterday at 12:00:00">];
                 inserted: null; outdated: []>>
    |}]
;;

let%expect_test {| Remove link to index 1. |} =
  let action = call_transient_log_remove_link ~index:1 ~key:"google"
  and should_fail = false in
  exec ~should_fail action;
  [%expect
    {|
    [DONE]: <id: 37; jsonrpc: "2.0";
              result:
               <all:
                 [<duration: null; duration_repr: null; index: 0;
                    label: "A first transient log!"; links: [];
                    meta: [<key: "location"; value: "Nantes">]; project: null;
                    sector: "a-new-sector"; start_date: "2025-03-01T11-00-00";
                    start_date_repr: "Yesterday at 11:00:00">,
                  <duration: 1500; duration_repr: "25m"; index: 1;
                    label: "A new label !";
                    links: [<key: "homepage"; value: "https://xvw.lol">];
                    meta:
                     [<key: "Foo"; value: "Bar">,
                      <key: "a meta"; value: "hehehe">];
                    project: "kohai"; sector: "programming";
                    start_date: "2025-03-01T12-00-00";
                    start_date_repr: "Yesterday at 12:00:00">];
                 inserted: null; outdated: []>>
    |}]
;;

let%expect_test {| Remove meta to index 1. |} =
  let action = call_transient_log_remove_meta ~index:1 ~key:"Foo"
  and should_fail = false in
  exec ~should_fail action;
  [%expect
    {|
    [DONE]: <id: 38; jsonrpc: "2.0";
              result:
               <all:
                 [<duration: null; duration_repr: null; index: 0;
                    label: "A first transient log!"; links: [];
                    meta: [<key: "location"; value: "Nantes">]; project: null;
                    sector: "a-new-sector"; start_date: "2025-03-01T11-00-00";
                    start_date_repr: "Yesterday at 11:00:00">,
                  <duration: 1500; duration_repr: "25m"; index: 1;
                    label: "A new label !";
                    links: [<key: "homepage"; value: "https://xvw.lol">];
                    meta: [<key: "a meta"; value: "hehehe">]; project: "kohai";
                    sector: "programming"; start_date: "2025-03-01T12-00-00";
                    start_date_repr: "Yesterday at 12:00:00">];
                 inserted: null; outdated: []>>
    |}]
;;

let%expect_test {| Fetch the state that should be empty. |} =
  let action = call_state_get
  and should_fail = false in
  exec ~should_fail action;
  [%expect
    {|
    [DONE]: <id: 39; jsonrpc: "2.0";
              result:
               <big_bang: null; duration: 0; end_of_world: null;
                 number_of_logs: 0>>
    |}]
;;

let%expect_test {| Promote the log index-1. |} =
  let action = call_transient_log_promote ~index:1
  and should_fail = false in
  exec ~should_fail action;
  [%expect
    {|
    [DONE]: <id: 40; jsonrpc: "2.0";
              result:
               <all:
                 [<duration: null; duration_repr: null; index: 0;
                    label: "A first transient log!"; links: [];
                    meta: [<key: "location"; value: "Nantes">]; project: null;
                    sector: "a-new-sector"; start_date: "2025-03-01T11-00-00";
                    start_date_repr: "Yesterday at 11:00:00">];
                 inserted: null; outdated: []>>
    |}]
;;

let%expect_test {| Fetch the state that should be filled. |} =
  let action = call_state_get
  and should_fail = false in
  exec ~should_fail action;
  [%expect
    {|
    [DONE]: <id: 41; jsonrpc: "2.0";
              result:
               <big_bang: "2025-03-01T12-00-00"; duration: 1500;
                 end_of_world: "2025-03-01T12-25-00"; number_of_logs: 1>>
    |}]
;;

let%expect_test {| Fetch the state for a regular sector. |} =
  let action = call_state_get_for_sector ~sector:"programming"
  and should_fail = false in
  exec ~should_fail action;
  [%expect
    {|
    [DONE]: <id: 42; jsonrpc: "2.0";
              result:
               <big_bang: "2025-03-01T12-00-00"; duration: 1500;
                 end_of_world: "2025-03-01T12-25-00"; number_of_logs: 1>>
    |}]
;;

let%expect_test {| Fetch the state for a regular project. |} =
  let action = call_state_get_for_project ~project:"kohai"
  and should_fail = false in
  exec ~should_fail action;
  [%expect
    {|
    [DONE]: <id: 43; jsonrpc: "2.0";
              result:
               <big_bang: "2025-03-01T12-00-00"; duration: 1500;
                 end_of_world: "2025-03-01T12-25-00"; number_of_logs: 1>>
    |}]
;;

let%expect_test {| Fetch the state for an inexesisting regular project. |} =
  let action = call_state_get_for_project ~project:"i-do-not-exists"
  and should_fail = false in
  exec ~should_fail action;
  [%expect
    {|
    [DONE]: <id: 44; jsonrpc: "2.0";
              result:
               <big_bang: null; duration: 0; end_of_world: null;
                 number_of_logs: 0>>
    |}]
;;

let%expect_test
    {|
        Promote the log index-0
        (should fail because no duration). |}
  =
  let action = call_transient_log_promote ~index:0
  and should_fail = true in
  exec ~should_fail action;
  [%expect
    {|
    [DONE]: <error:
              <body:
                "{"jsonrpc": "2.0", "method": "kohai/transient-log/action", "id": 45, "params": {"ctor":"promote","value":{"index":0}}}";
                code: -32010; data: "[transient log#0] not found";
                message: "Server error">;
              id: 45; jsonrpc: "2.0">
    |}]
;;

let%expect_test {| Get the list of sector counter should be increased. |} =
  let action = call_sector_list
  and should_fail = false in
  exec ~should_fail action;
  [%expect
    {|
    [DONE]: <id: 46; jsonrpc: "2.0";
              result:
               [<counter: 0; description: null; name: "a-new-sector">,
                <counter: 1; description: "Category related to programming";
                  name: "programming">,
                <counter: 0; description: "A description"; name: "visual">]>
    |}]
;;

let%expect_test {| Try to delete a sector (that can't be deleted). |} =
  let action = call_sector_delete ~name:"programming"
  and should_fail = false in
  exec ~should_fail action;
  [%expect
    {|
    [DONE]: <id: 47; jsonrpc: "2.0";
              result:
               [<counter: 0; description: null; name: "a-new-sector">,
                <counter: 1; description: "Category related to programming";
                  name: "programming">,
                <counter: 0; description: "A description"; name: "visual">]>
    |}]
;;

let%expect_test {| Get the list of project counter should be increased. |} =
  let action = call_project_list
  and should_fail = false in
  exec ~should_fail action;
  [%expect
    {|
    [DONE]: <id: 48; jsonrpc: "2.0";
              result:
               [<counter: 0;
                  description: "My personnal website built with OCaml";
                  name: "capsule">,
                <counter: 1; description: "An opinionated timetracker";
                  name: "kohai">]>
    |}]
;;

let%expect_test {| Try to delete a project (that can't be deleted). |} =
  let action = call_project_delete ~name:"kohai"
  and should_fail = false in
  exec ~should_fail action;
  [%expect
    {|
    [DONE]: <id: 49; jsonrpc: "2.0";
              result:
               [<counter: 0;
                  description: "My personnal website built with OCaml";
                  name: "capsule">,
                <counter: 1; description: "An opinionated timetracker";
                  name: "kohai">]>
    |}]
;;

let%expect_test {| Get the list of last logs. |} =
  let action = call_log_last
  and should_fail = false in
  exec ~should_fail action;
  [%expect
    {|
    [DONE]: <id: 50; jsonrpc: "2.0";
              result:
               [<duration: 1500; duration_repr: "25m";
                  id: "3b2c3def-c539-538f-9619-9c15c30500af";
                  label: "A new label !";
                  links: [<key: "homepage"; value: "https://xvw.lol">];
                  meta: [<key: "a meta"; value: "hehehe">]; project: "kohai";
                  sector: "programming"; start_date: "2025-03-01T12-00-00";
                  start_date_repr: "Yesterday at 12:00:00">]>
    |}]
;;

let%expect_test {| Close the remaining transient log. |} =
  let action = call_transient_log_stop_recording ~index:0 ?duration:None
  and should_fail = false in
  exec ~should_fail action;
  [%expect
    {|
    [DONE]: <id: 51; jsonrpc: "2.0";
              result:
               <all:
                 [<duration: 46800; duration_repr: "13h"; index: 0;
                    label: "A first transient log!"; links: [];
                    meta: [<key: "location"; value: "Nantes">]; project: null;
                    sector: "a-new-sector"; start_date: "2025-03-01T11-00-00";
                    start_date_repr: "Yesterday at 11:00:00">];
                 inserted: null; outdated: []>>
    |}]
;;

let%expect_test {| Promote the remaining transient log. |} =
  let action = call_transient_log_promote ~index:0
  and should_fail = false in
  exec ~should_fail action;
  [%expect
    {|
    [DONE]: <id: 52; jsonrpc: "2.0";
              result: <all: []; inserted: null; outdated: []>>
    |}]
;;

let%expect_test {| Get the list of last logs. |} =
  let action = call_log_last
  and should_fail = false in
  exec ~should_fail action;
  [%expect
    {|
    [DONE]: <id: 53; jsonrpc: "2.0";
              result:
               [<duration: 1500; duration_repr: "25m";
                  id: "3b2c3def-c539-538f-9619-9c15c30500af";
                  label: "A new label !";
                  links: [<key: "homepage"; value: "https://xvw.lol">];
                  meta: [<key: "a meta"; value: "hehehe">]; project: "kohai";
                  sector: "programming"; start_date: "2025-03-01T12-00-00";
                  start_date_repr: "Yesterday at 12:00:00">,
                <duration: 46800; duration_repr: "13h";
                  id: "62d4a935-45f2-502e-864c-d0cf62ec8146";
                  label: "A first transient log!"; links: [];
                  meta: [<key: "location"; value: "Nantes">]; project: null;
                  sector: "a-new-sector"; start_date: "2025-03-01T11-00-00";
                  start_date_repr: "Yesterday at 11:00:00">]>
    |}]
;;

let%expect_test {| Get the list of last logs for sector programming. |} =
  let action = call_log_last_for_sector ~sector:"programming"
  and should_fail = false in
  exec ~should_fail action;
  [%expect
    {|
    [DONE]: <id: 54; jsonrpc: "2.0";
              result:
               [<duration: 1500; duration_repr: "25m";
                  id: "3b2c3def-c539-538f-9619-9c15c30500af";
                  label: "A new label !";
                  links: [<key: "homepage"; value: "https://xvw.lol">];
                  meta: [<key: "a meta"; value: "hehehe">]; project: "kohai";
                  sector: "programming"; start_date: "2025-03-01T12-00-00";
                  start_date_repr: "Yesterday at 12:00:00">]>
    |}]
;;

let%expect_test {| Get the list of last logs for project kohai. |} =
  let action = call_log_last_for_project ~project:"kohai"
  and should_fail = false in
  exec ~should_fail action;
  [%expect
    {|
    [DONE]: <id: 55; jsonrpc: "2.0";
              result:
               [<duration: 1500; duration_repr: "25m";
                  id: "3b2c3def-c539-538f-9619-9c15c30500af";
                  label: "A new label !";
                  links: [<key: "homepage"; value: "https://xvw.lol">];
                  meta: [<key: "a meta"; value: "hehehe">]; project: "kohai";
                  sector: "programming"; start_date: "2025-03-01T12-00-00";
                  start_date_repr: "Yesterday at 12:00:00">]>
    |}]
;;

let%expect_test {| Get the list of last logs for sector a-new-sector. |} =
  let action = call_log_last_for_sector ~sector:"a-new-sector"
  and should_fail = false in
  exec ~should_fail action;
  [%expect
    {|
    [DONE]: <id: 56; jsonrpc: "2.0";
              result:
               [<duration: 46800; duration_repr: "13h";
                  id: "62d4a935-45f2-502e-864c-d0cf62ec8146";
                  label: "A first transient log!"; links: [];
                  meta: [<key: "location"; value: "Nantes">]; project: null;
                  sector: "a-new-sector"; start_date: "2025-03-01T11-00-00";
                  start_date_repr: "Yesterday at 11:00:00">]>
    |}]
;;

let%expect_test {| Get the transient log list. |} =
  let action = call_transient_log_list
  and should_fail = false in
  exec ~should_fail action;
  [%expect {| [DONE]: <id: 57; jsonrpc: "2.0"; result: []> |}]
;;

let%expect_test {| Retreive state (to see if it was removed). |} =
  let action = call_state_get
  and should_fail = false in
  exec ~should_fail action;
  [%expect
    {|
    [DONE]: <id: 58; jsonrpc: "2.0";
              result:
               <big_bang: "2025-03-01T11-00-00"; duration: 48300;
                 end_of_world: "2025-03-02T00-00-00"; number_of_logs: 2>>
    |}]
;;

let%expect_test {| Unpromote a promoted log. |} =
  let action = call_log_unpromote ~uuid:"62d4a935-45f2-502e-864c-d0cf62ec8146"
  and should_fail = false in
  exec ~should_fail action;
  [%expect
    {|
    [DONE]: <id: 59; jsonrpc: "2.0";
              result:
               [<duration: 46800; duration_repr: "13h"; index: 0;
                  label: "A first transient log!"; links: [];
                  meta: [<key: "location"; value: "Nantes">]; project: null;
                  sector: "a-new-sector"; start_date: "2025-03-01T11-00-00";
                  start_date_repr: "Yesterday at 11:00:00">]>
    |}]
;;

let%expect_test {| Get the list of last logs (to see if it was removed). |} =
  let action = call_log_last
  and should_fail = false in
  exec ~should_fail action;
  [%expect
    {|
    [DONE]: <id: 60; jsonrpc: "2.0";
              result:
               [<duration: 1500; duration_repr: "25m";
                  id: "3b2c3def-c539-538f-9619-9c15c30500af";
                  label: "A new label !";
                  links: [<key: "homepage"; value: "https://xvw.lol">];
                  meta: [<key: "a meta"; value: "hehehe">]; project: "kohai";
                  sector: "programming"; start_date: "2025-03-01T12-00-00";
                  start_date_repr: "Yesterday at 12:00:00">]>
    |}]
;;

let%expect_test {| Get the list of last logs for sector a-new-sector. |} =
  let action = call_log_last_for_sector ~sector:"a-new-sector"
  and should_fail = false in
  exec ~should_fail action;
  [%expect {| [DONE]: <id: 61; jsonrpc: "2.0"; result: []> |}]
;;

let%expect_test {| Retreive state. |} =
  let action = call_state_get
  and should_fail = false in
  exec ~should_fail action;
  [%expect
    {|
    [DONE]: <id: 62; jsonrpc: "2.0";
              result:
               <big_bang: "2025-03-01T11-00-00"; duration: 1500;
                 end_of_world: "2025-03-02T00-00-00"; number_of_logs: 1>>
    |}]
;;

let%expect_test {| Retreive state for a sector `a-new-sector` |} =
  let action = call_state_get_for_sector ~sector:"a-new-sector"
  and should_fail = false in
  exec ~should_fail action;
  [%expect
    {|
    [DONE]: <id: 63; jsonrpc: "2.0";
              result:
               <big_bang: "2025-03-01T11-00-00"; duration: 0;
                 end_of_world: "2025-03-02T00-00-00"; number_of_logs: 0>>
    |}]
;;
