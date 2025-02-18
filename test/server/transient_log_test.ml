open Kohai_server
open Util

let input ~id ?params meth =
  let i = !id in
  let () = incr id in
  request_input ~id:i ?params meth
;;

let opt f = function
  | None -> `Null
  | Some x -> f x
;;

let opt_string k = opt (fun x -> `String x) k
let opt_int k = opt (fun x -> `Int x) k

let record ?start_date ?project ~sector label =
  let x =
    `Variant
      ( "Record"
      , Some
          (`Assoc
              [ "start_date", opt_string start_date
              ; "project", opt_string project
              ; "sector", `String sector
              ; "label", `String label
              ]) )
  in
  Yojson.to_string x
;;

let rewrite ~index ?start_date ?project ~sector label =
  let x =
    `Variant
      ( "Rewrite"
      , Some
          (`Assoc
              [ "index", `Int index
              ; "start_date", opt_string start_date
              ; "project", opt_string project
              ; "sector", `String sector
              ; "label", `String label
              ]) )
  in
  Yojson.to_string x
;;

let stop ?duration index =
  let x =
    `Variant
      ( "Stop_recording"
      , Some (`Assoc [ "index", `Int index; "duration", opt_int duration ]) )
  in
  Yojson.to_string x
;;

let delete ~index =
  let x = `Variant ("Delete", Some (`Assoc [ "index", `Int index ])) in
  Yojson.to_string x
;;

let default_sectors =
  [ {|<name: "programming"; description: "logs related to programming">|}
  ; {|<name: "visual"; description: "logs related to art">|}
  ; {|<name: "music"; description: "logs related to music">|}
  ]
  |> String.concat "\n"
;;

let default_fs =
  Virtfs.(
    from_list
      [ dir
          "supervised"
          [ dir "list" [ file ~content:default_sectors "sectors.rens" ] ]
      ])
;;

let default_dt =
  let open Kohai_core.Datetime in
  match make ~time:(10, 0, 0) ~year:2025 ~month:Feb ~day:13 () with
  | Ok dt -> dt
  | _ -> failwith "Invalid date" (* should not happen. *)
;;

let%expect_test "get transient logs when file are empty" =
  let module FS =
    Virtfs.Make (struct
      let fs = default_fs
      let now = default_dt
    end)
  in
  let module H = Kohai_core.Eff.Handler (FS) in
  let id = ref 0 in
  let supervise =
    "kohai/supervision/set"
    |> input ~id ~params:{|"/supervised"|}
    |> Jsonrpc.run ~services:Services.all
    |> Kohai_core.Eff.handle (module H)
  in
  let get_logs =
    "kohai/transient-log/list"
    |> input ~id
    |> Jsonrpc.run ~services:Services.all
    |> Kohai_core.Eff.handle (module H)
  in
  List.iter
    (fun result -> result |> request_dump |> print_endline)
    [ supervise; get_logs ];
  [%expect
    {|
    {id = 0; jsonrpc = "2.0"; result = "/supervised"}
    {id = 1; jsonrpc = "2.0"; result = []}
    |}]
;;

let%expect_test "end-to-end test - 1" =
  let module FS =
    Virtfs.Make (struct
      let fs = default_fs
      let now = default_dt
    end)
  in
  let module H = Kohai_core.Eff.Handler (FS) in
  let id = ref 0 in
  let supervise =
    "kohai/supervision/set"
    |> input ~id ~params:{|"/supervised"|}
    |> Jsonrpc.run ~services:Services.all
    |> Kohai_core.Eff.handle (module H)
  in
  let get_logs =
    "kohai/transient-log/list"
    |> input ~id
    |> Jsonrpc.run ~services:Services.all
    |> Kohai_core.Eff.handle (module H)
  in
  let first_record =
    let params = record ~sector:"programming" "test of a log" in
    "kohai/transient-log/action"
    |> input ~id ~params
    |> Jsonrpc.run ~services:Services.all
    |> Kohai_core.Eff.handle (module H)
  in
  let () = FS.manip_time Kohai_core.Datetime.succ_hour in
  let second_record =
    let params =
      record ~project:"kohai" ~sector:"not-known" "test of an other log"
    in
    "kohai/transient-log/action"
    |> input ~id ~params
    |> Jsonrpc.run ~services:Services.all
    |> Kohai_core.Eff.handle (module H)
  in
  let get_sectors =
    "kohai/sector/list"
    |> input ~id
    |> Jsonrpc.run ~services:Services.all
    |> Kohai_core.Eff.handle (module H)
  in
  let get_first =
    "kohai/transient-log/get"
    |> input ~id ~params:"0"
    |> Jsonrpc.run ~services:Services.all
    |> Kohai_core.Eff.handle (module H)
  in
  let get_second =
    "kohai/transient-log/get"
    |> input ~id ~params:"1"
    |> Jsonrpc.run ~services:Services.all
    |> Kohai_core.Eff.handle (module H)
  in
  let () = FS.manip_time Kohai_core.Datetime.succ_hour in
  let close_first =
    let params = stop 0 in
    "kohai/transient-log/action"
    |> input ~id ~params
    |> Jsonrpc.run ~services:Services.all
    |> Kohai_core.Eff.handle (module H)
  in
  let close_second =
    let params = stop ~duration:60 1 in
    "kohai/transient-log/action"
    |> input ~id ~params
    |> Jsonrpc.run ~services:Services.all
    |> Kohai_core.Eff.handle (module H)
  in
  let get_projects =
    "kohai/project/list"
    |> input ~id
    |> Jsonrpc.run ~services:Services.all
    |> Kohai_core.Eff.handle (module H)
  in
  let third_record =
    let params =
      record ~project:"capsule" ~sector:"visual" "Make visual stuff on Capsule"
    in
    "kohai/transient-log/action"
    |> input ~id ~params
    |> Jsonrpc.run ~services:Services.all
    |> Kohai_core.Eff.handle (module H)
  in
  let get_projects_with_capsule =
    "kohai/project/list"
    |> input ~id
    |> Jsonrpc.run ~services:Services.all
    |> Kohai_core.Eff.handle (module H)
  in
  let rewrite_third_record =
    let params =
      rewrite
        ~project:"capsule"
        ~sector:"visual"
        ~index:2
        "Make visual stuff on Capsule (rewritted)"
    in
    "kohai/transient-log/action"
    |> input ~id ~params
    |> Jsonrpc.run ~services:Services.all
    |> Kohai_core.Eff.handle (module H)
  in
  let remove_third_record =
    let params = delete ~index:2 in
    "kohai/transient-log/action"
    |> input ~id ~params
    |> Jsonrpc.run ~services:Services.all
    |> Kohai_core.Eff.handle (module H)
  in
  List.iter
    (fun result -> result |> request_dump |> print_endline)
    [ supervise
    ; get_logs
    ; first_record
    ; second_record
    ; get_sectors
    ; get_first
    ; get_second
    ; close_first
    ; close_second
    ; get_projects
    ; third_record
    ; get_projects_with_capsule
    ; rewrite_third_record
    ; remove_third_record
    ];
  [%expect
    {|
    {id = 0; jsonrpc = "2.0"; result = "/supervised"}
    {id = 1; jsonrpc = "2.0"; result = []}
    {id = 2; jsonrpc = "2.0";
      result =
       {all =
         [{duration = <null>; index = 0; label = "test of a log"; meta = [];
            project = <null>; sector = "programming";
            start_date = "2025-02-12T11-00-00"}];
         inserted =
          {duration = <null>; index = -1; label = "test of a log"; meta = [];
            project = <null>; sector = "programming";
            start_date = "2025-02-12T11-00-00"};
         outdated = []}}
    {id = 3; jsonrpc = "2.0";
      result =
       {all =
         [{duration = <null>; index = 0; label = "test of a log"; meta = [];
            project = <null>; sector = "programming";
            start_date = "2025-02-12T11-00-00"};
          {duration = <null>; index = 1; label = "test of an other log";
            meta = []; project = "kohai"; sector = "not-known";
            start_date = "2025-02-12T12-00-00"}];
         inserted =
          {duration = <null>; index = -1; label = "test of an other log";
            meta = []; project = "kohai"; sector = "not-known";
            start_date = "2025-02-12T12-00-00"};
         outdated =
          [{duration = <null>; index = 0; label = "test of a log"; meta = [];
             project = <null>; sector = "programming";
             start_date = "2025-02-12T11-00-00"}]}}
    {id = 4; jsonrpc = "2.0";
      result =
       [{counter = 0; description = "logs related to music"; name = "music"};
        {counter = 0; description = <null>; name = "not-known"};
        {counter = 0; description = "logs related to programming";
          name = "programming"};
        {counter = 0; description = "logs related to art"; name = "visual"}]}
    {id = 5; jsonrpc = "2.0";
      result =
       {duration = <null>; index = 0; label = "test of a log"; meta = [];
         project = <null>; sector = "programming";
         start_date = "2025-02-12T11-00-00"}}
    {id = 6; jsonrpc = "2.0";
      result =
       {duration = <null>; index = 1; label = "test of an other log"; meta = [];
         project = "kohai"; sector = "not-known";
         start_date = "2025-02-12T12-00-00"}}
    {id = 7; jsonrpc = "2.0";
      result =
       {all =
         [{duration = 7200; index = 0; label = "test of a log"; meta = [];
            project = <null>; sector = "programming";
            start_date = "2025-02-12T11-00-00"};
          {duration = <null>; index = 1; label = "test of an other log";
            meta = []; project = "kohai"; sector = "not-known";
            start_date = "2025-02-12T12-00-00"}];
         inserted = <null>; outdated = []}}
    {id = 8; jsonrpc = "2.0";
      result =
       {all =
         [{duration = 7200; index = 0; label = "test of a log"; meta = [];
            project = <null>; sector = "programming";
            start_date = "2025-02-12T11-00-00"};
          {duration = 3600; index = 1; label = "test of an other log"; meta = [];
            project = "kohai"; sector = "not-known";
            start_date = "2025-02-12T12-00-00"}];
         inserted = <null>; outdated = []}}
    {id = 9; jsonrpc = "2.0";
      result = [{counter = 0; description = <null>; name = "kohai"}]}
    {id = 10; jsonrpc = "2.0";
      result =
       {all =
         [{duration = 7200; index = 0; label = "test of a log"; meta = [];
            project = <null>; sector = "programming";
            start_date = "2025-02-12T11-00-00"};
          {duration = 3600; index = 1; label = "test of an other log"; meta = [];
            project = "kohai"; sector = "not-known";
            start_date = "2025-02-12T12-00-00"};
          {duration = <null>; index = 2; label = "Make visual stuff on Capsule";
            meta = []; project = "capsule"; sector = "visual";
            start_date = "2025-02-12T13-00-00"}];
         inserted =
          {duration = <null>; index = -1; label = "Make visual stuff on Capsule";
            meta = []; project = "capsule"; sector = "visual";
            start_date = "2025-02-12T13-00-00"};
         outdated = []}}
    {id = 11; jsonrpc = "2.0";
      result =
       [{counter = 0; description = <null>; name = "capsule"};
        {counter = 0; description = <null>; name = "kohai"}]}
    {id = 12; jsonrpc = "2.0";
      result =
       {all =
         [{duration = 7200; index = 0; label = "test of a log"; meta = [];
            project = <null>; sector = "programming";
            start_date = "2025-02-12T11-00-00"};
          {duration = 3600; index = 1; label = "test of an other log"; meta = [];
            project = "kohai"; sector = "not-known";
            start_date = "2025-02-12T12-00-00"};
          {duration = <null>; index = 2;
            label = "Make visual stuff on Capsule (rewritted)"; meta = [];
            project = "capsule"; sector = "visual";
            start_date = "2025-02-12T13-00-00"}];
         inserted = <null>; outdated = []}}
    {id = 13; jsonrpc = "2.0";
      result =
       {all =
         [{duration = 7200; index = 0; label = "test of a log"; meta = [];
            project = <null>; sector = "programming";
            start_date = "2025-02-12T11-00-00"};
          {duration = 3600; index = 1; label = "test of an other log"; meta = [];
            project = "kohai"; sector = "not-known";
            start_date = "2025-02-12T12-00-00"}];
         inserted = <null>; outdated = []}}
    |}]
;;

let dump_list x =
  List.iter (fun result -> result |> request_dump |> print_endline) x
;;

let%expect_test "end-to-end test - 2" =
  let module FS =
    Virtfs.Make (struct
      let fs = default_fs
      let now = default_dt
    end)
  in
  let module H = Kohai_core.Eff.Handler (FS) in
  let id = ref 0 in
  let supervise =
    "kohai/supervision/set"
    |> input ~id ~params:{|"/supervised"|}
    |> Jsonrpc.run ~services:Services.all
    |> Kohai_core.Eff.handle (module H)
  in
  dump_list [ supervise ];
  [%expect {| {id = 0; jsonrpc = "2.0"; result = "/supervised"} |}];
  let first_record =
    let params = record ~sector:"programming" "log 1" in
    "kohai/transient-log/action"
    |> input ~id ~params
    |> Jsonrpc.run ~services:Services.all
    |> Kohai_core.Eff.handle (module H)
  in
  dump_list [ first_record ];
  [%expect
    {|
    {id = 1; jsonrpc = "2.0";
      result =
       {all =
         [{duration = <null>; index = 0; label = "log 1"; meta = [];
            project = <null>; sector = "programming";
            start_date = "2025-02-12T11-00-00"}];
         inserted =
          {duration = <null>; index = -1; label = "log 1"; meta = [];
            project = <null>; sector = "programming";
            start_date = "2025-02-12T11-00-00"};
         outdated = []}}
    |}];
  let () = FS.manip_time Kohai_core.Datetime.succ_hour in
  let second_record =
    let params = record ~project:"kohai" ~sector:"not-known" "log 2" in
    "kohai/transient-log/action"
    |> input ~id ~params
    |> Jsonrpc.run ~services:Services.all
    |> Kohai_core.Eff.handle (module H)
  in
  dump_list [ second_record ];
  [%expect
    {|
    {id = 2; jsonrpc = "2.0";
      result =
       {all =
         [{duration = <null>; index = 0; label = "log 1"; meta = [];
            project = <null>; sector = "programming";
            start_date = "2025-02-12T11-00-00"};
          {duration = <null>; index = 1; label = "log 2"; meta = [];
            project = "kohai"; sector = "not-known";
            start_date = "2025-02-12T12-00-00"}];
         inserted =
          {duration = <null>; index = -1; label = "log 2"; meta = [];
            project = "kohai"; sector = "not-known";
            start_date = "2025-02-12T12-00-00"};
         outdated =
          [{duration = <null>; index = 0; label = "log 1"; meta = [];
             project = <null>; sector = "programming";
             start_date = "2025-02-12T11-00-00"}]}}
    |}];
  let () = FS.manip_time Kohai_core.Datetime.succ_hour in
  let third_record =
    let params = record ~project:"capsule" ~sector:"visual" "log 3" in
    "kohai/transient-log/action"
    |> input ~id ~params
    |> Jsonrpc.run ~services:Services.all
    |> Kohai_core.Eff.handle (module H)
  in
  dump_list [ third_record ];
  [%expect
    {|
    {id = 3; jsonrpc = "2.0";
      result =
       {all =
         [{duration = <null>; index = 0; label = "log 1"; meta = [];
            project = <null>; sector = "programming";
            start_date = "2025-02-12T11-00-00"};
          {duration = <null>; index = 1; label = "log 2"; meta = [];
            project = "kohai"; sector = "not-known";
            start_date = "2025-02-12T12-00-00"};
          {duration = <null>; index = 2; label = "log 3"; meta = [];
            project = "capsule"; sector = "visual";
            start_date = "2025-02-12T13-00-00"}];
         inserted =
          {duration = <null>; index = -1; label = "log 3"; meta = [];
            project = "capsule"; sector = "visual";
            start_date = "2025-02-12T13-00-00"};
         outdated =
          [{duration = <null>; index = 0; label = "log 1"; meta = [];
             project = <null>; sector = "programming";
             start_date = "2025-02-12T11-00-00"};
           {duration = <null>; index = 1; label = "log 2"; meta = [];
             project = "kohai"; sector = "not-known";
             start_date = "2025-02-12T12-00-00"}]}}
    |}];
  let () = FS.manip_time Kohai_core.Datetime.succ_hour in
  let close_first =
    let params = stop 0 in
    "kohai/transient-log/action"
    |> input ~id ~params
    |> Jsonrpc.run ~services:Services.all
    |> Kohai_core.Eff.handle (module H)
  in
  dump_list [ close_first ];
  [%expect
    {|
    {id = 4; jsonrpc = "2.0";
      result =
       {all =
         [{duration = 10800; index = 0; label = "log 1"; meta = [];
            project = <null>; sector = "programming";
            start_date = "2025-02-12T11-00-00"};
          {duration = <null>; index = 1; label = "log 2"; meta = [];
            project = "kohai"; sector = "not-known";
            start_date = "2025-02-12T12-00-00"};
          {duration = <null>; index = 2; label = "log 3"; meta = [];
            project = "capsule"; sector = "visual";
            start_date = "2025-02-12T13-00-00"}];
         inserted = <null>; outdated = []}}
    |}];
  let () = FS.manip_time Kohai_core.Datetime.succ_hour in
  let get_list =
    "kohai/transient-log/list"
    |> input ~id
    |> Jsonrpc.run ~services:Services.all
    |> Kohai_core.Eff.handle (module H)
  in
  dump_list [ get_list ];
  let fourth_record =
    let params = record ~project:"kohai" ~sector:"programming" "log 4" in
    "kohai/transient-log/action"
    |> input ~id ~params
    |> Jsonrpc.run ~services:Services.all
    |> Kohai_core.Eff.handle (module H)
  in
  dump_list [ fourth_record ];
  [%expect
    {|
    {id = 5; jsonrpc = "2.0";
      result =
       [{duration = 10800; index = 0; label = "log 1"; meta = [];
          project = <null>; sector = "programming";
          start_date = "2025-02-12T11-00-00"};
        {duration = <null>; index = 1; label = "log 2"; meta = [];
          project = "kohai"; sector = "not-known";
          start_date = "2025-02-12T12-00-00"};
        {duration = <null>; index = 2; label = "log 3"; meta = [];
          project = "capsule"; sector = "visual";
          start_date = "2025-02-12T13-00-00"}]}
    {id = 6; jsonrpc = "2.0";
      result =
       {all =
         [{duration = 10800; index = 0; label = "log 1"; meta = [];
            project = <null>; sector = "programming";
            start_date = "2025-02-12T11-00-00"};
          {duration = <null>; index = 1; label = "log 2"; meta = [];
            project = "kohai"; sector = "not-known";
            start_date = "2025-02-12T12-00-00"};
          {duration = <null>; index = 2; label = "log 3"; meta = [];
            project = "capsule"; sector = "visual";
            start_date = "2025-02-12T13-00-00"};
          {duration = <null>; index = 3; label = "log 4"; meta = [];
            project = "kohai"; sector = "programming";
            start_date = "2025-02-12T15-00-00"}];
         inserted =
          {duration = <null>; index = -1; label = "log 4"; meta = [];
            project = "kohai"; sector = "programming";
            start_date = "2025-02-12T15-00-00"};
         outdated =
          [{duration = <null>; index = 1; label = "log 2"; meta = [];
             project = "kohai"; sector = "not-known";
             start_date = "2025-02-12T12-00-00"};
           {duration = <null>; index = 2; label = "log 3"; meta = [];
             project = "capsule"; sector = "visual";
             start_date = "2025-02-12T13-00-00"}]}}
    |}]
;;
