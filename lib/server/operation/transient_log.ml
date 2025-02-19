module M = Kohai_model.Transient_log

let all ?body ?id (module H : Eff.HANDLER) =
  let cwd = Global.ensure_supervision ?body ?id (module H) () in
  let file = Kohai_model.Resolver.transient_logs ~cwd in
  let content = Eff.read_file (module H) file in
  M.from_file_content content
;;

let list ?body ?id (module H : Eff.HANDLER) () =
  let now = Eff.now (module H) in
  now, all ?body ?id (module H) |> M.sort
;;

let get ?body ?id (module H : Eff.HANDLER) index =
  all ?body ?id (module H) |> List.find_opt (M.has_index index)
;;

let store_missing_sector ?body ?id (module H : Eff.HANDLER) sector =
  sector |> Kohai_model.Described_item.make |> Sector.save ?body ?id (module H)
;;

let store_missing_project ?body ?id (module H : Eff.HANDLER) project =
  project
  |> Kohai_model.Described_item.make
  |> Project.save ?body ?id (module H)
;;

let store_missing_data ?body ?id (module H : Eff.HANDLER) ~sector ~project =
  let _ = store_missing_sector ?body ?id (module H) sector
  and () =
    match project with
    | Some project ->
      store_missing_project ?body ?id (module H) project |> ignore
    | None -> ()
  in
  ()
;;

let action ?body ?id (module H : Eff.HANDLER) operation =
  let cwd = Global.ensure_supervision ?body ?id (module H) () in
  let file = Kohai_model.Resolver.transient_logs ~cwd in
  let now = Eff.now (module H) in
  match operation with
  | M.Record { date_query; project; sector; label } ->
    let () = store_missing_data ?body ?id (module H) ~sector ~project in
    let transients = all ?body ?id (module H) in
    let start_date = Datetime.Query.resolve now date_query in
    let transient = M.make ~start_date ~project ~sector ~label in
    let result = M.to_result ~inserted:transient transients in
    let content = M.dump result in
    let () = Eff.write_file (module H) file content in
    now, result
  | M.Stop_recording { index; duration } ->
    let transients =
      all ?body ?id (module H)
      |> List.map (fun log ->
        if M.has_index index log
        then M.finalize_duration now log duration
        else log)
    in
    let result = M.to_result transients in
    let content = M.dump result in
    let () = Eff.write_file (module H) file content in
    now, result
  | M.Rewrite { index; date_query; project; sector; label } ->
    let () = store_missing_data ?body ?id (module H) ~sector ~project in
    let start_date = Datetime.Query.resolve now date_query in
    let patched_log = M.make ~start_date ~project ~sector ~label in
    let transients =
      all ?body ?id (module H)
      |> List.map (fun log ->
        if M.has_index index log then patched_log else log)
    in
    let result = M.to_result transients in
    let content = M.dump result in
    let () = Eff.write_file (module H) file content in
    now, result
  | M.Delete { index } ->
    let transients =
      all ?body ?id (module H)
      |> List.filter (fun log -> not (M.has_index index log))
    in
    let result = M.to_result transients in
    let content = M.dump result in
    let () = Eff.write_file (module H) file content in
    now, result
  | M.Add_meta { index; key; value } ->
    let transients =
      all ?body ?id (module H)
      |> List.map (fun log ->
        if M.has_index index log then M.add_meta ~key ~value log else log)
    in
    let result = M.to_result transients in
    let content = M.dump result in
    let () = Eff.write_file (module H) file content in
    now, result
  | M.Remove_meta { index; key } ->
    let transients =
      all ?body ?id (module H)
      |> List.map (fun log ->
        if M.has_index index log then M.remove_meta ~key log else log)
    in
    let result = M.to_result transients in
    let content = M.dump result in
    let () = Eff.write_file (module H) file content in
    now, result
;;
