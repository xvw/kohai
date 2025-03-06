module M = Kohai_model.Transient_log
module L = Kohai_model.Log

let all ?id ~body (module H : Eff.HANDLER) =
  let cwd = Global.ensure_supervision ?id ~body (module H) () in
  let file = Kohai_model.Resolver.transient_logs ~cwd in
  let content = Eff.read_file (module H) file in
  M.from_file_content content
;;

let list ?id ~body (module H : Eff.HANDLER) () =
  let now = Eff.now (module H) in
  now, all ?id ~body (module H) |> M.sort
;;

let get ?id ~body (module H : Eff.HANDLER) index =
  all ?id ~body (module H) |> List.find_opt (M.has_index index)
;;

let store_missing_sector ?id ~body (module H : Eff.HANDLER) sector =
  sector |> Kohai_model.Described_item.make |> Sector.save ?id ~body (module H)
;;

let store_missing_project ?id ~body (module H : Eff.HANDLER) project =
  project
  |> Kohai_model.Described_item.make
  |> Project.save ?id ~body (module H)
;;

let store_missing_data ?id ~body (module H : Eff.HANDLER) ~sector ~project =
  let _ = store_missing_sector ?id ~body (module H) sector
  and () =
    match project with
    | Some project ->
      store_missing_project ?id ~body (module H) project |> ignore
    | None -> ()
  in
  ()
;;

let adding ?id ~body (module H : Eff.HANDLER) add file now index key value =
  let transients =
    all ?id ~body (module H)
    |> List.map (fun log ->
      if M.has_index index log then add ~key ~value log else log)
  in
  let result = M.to_result transients in
  let content = M.dump result in
  let () = Eff.write_file (module H) file content in
  now, result
;;

let removing ?id ~body (module H : Eff.HANDLER) remove file now index key =
  let transients =
    all ?id ~body (module H)
    |> List.map (fun log ->
      if M.has_index index log then remove ~key log else log)
  in
  let result = M.to_result transients in
  let content = M.dump result in
  let () = Eff.write_file (module H) file content in
  now, result
;;

let save ?id ~body (module H : Eff.HANDLER) file now transient =
  let sector = M.sector transient in
  let project = M.project transient in
  let () = store_missing_data ?id ~body (module H) ~sector ~project in
  let transients = all ?id ~body (module H) in
  let result = M.to_result ~inserted:transient transients in
  let content = M.dump result in
  let () = Eff.write_file (module H) file content in
  now, result
;;

let action_record
      ?id
      ~body
      (module H : Eff.HANDLER)
      file
      now
      date_query
      project
      sector
      label
  =
  let () = store_missing_data ?id ~body (module H) ~sector ~project in
  let start_date = Datetime.Query.resolve now date_query in
  let transient = M.make ~start_date ~project ~sector ~label () in
  save ?id ~body (module H) file now transient
;;

let stop_recording_action
      ?id
      ~body
      (module H : Eff.HANDLER)
      file
      now
      index
      duration
  =
  let transients =
    all ?id ~body (module H)
    |> List.map (fun log ->
      if M.has_index index log
      then M.finalize_duration now log duration
      else log)
  in
  let result = M.to_result transients in
  let content = M.dump result in
  let () = Eff.write_file (module H) file content in
  now, result
;;

let rewrite_action
      ?id
      ~body
      (module H : Eff.HANDLER)
      file
      now
      index
      date_query
      project
      sector
      label
  =
  let () = store_missing_data ?id ~body (module H) ~sector ~project in
  let start_date = Datetime.Query.resolve now date_query in
  let patched_log = M.make ~start_date ~project ~sector ~label () in
  let transients =
    all ?id ~body (module H)
    |> List.map (fun log -> if M.has_index index log then patched_log else log)
  in
  let result = M.to_result transients in
  let content = M.dump result in
  let () = Eff.write_file (module H) file content in
  now, result
;;

let delete_action ?id ~body (module H : Eff.HANDLER) file now index =
  let transients =
    all ?id ~body (module H)
    |> List.filter (fun log -> not (M.has_index index log))
  in
  let result = M.to_result transients in
  let content = M.dump result in
  let () = Eff.write_file (module H) file content in
  now, result
;;

let add_meta_action ?id ~body (module H : Eff.HANDLER) file now index key value =
  adding ?id ~body (module H) M.add_meta file now index key value
;;

let add_link_action ?id ~body (module H : Eff.HANDLER) file now index key value =
  adding ?id ~body (module H) M.add_link file now index key value
;;

let remove_meta_action ?id ~body (module H : Eff.HANDLER) file now index key =
  removing ?id ~body (module H) M.remove_meta file now index key
;;

let remove_link_action ?id ~body (module H : Eff.HANDLER) file now index key =
  removing ?id ~body (module H) M.remove_link file now index key
;;

let promote_action ?id ~body (module H : Eff.HANDLER) file now cwd index =
  let tl = get ?id ~body (module H) index in
  let log = Option.bind tl (fun x -> L.from_transient_log x) in
  match log with
  | Some log ->
    (* Dump log in the related file. *)
    let () = Log.promote ?id ~body (module H) cwd log in
    (* Remove the current transient log. *)
    delete_action ?id ~body (module H) file now index
  | None ->
    Eff.raise (module H) (Error.no_related_transient_log ?id ~body index)
;;

let action ?id ~body (module H : Eff.HANDLER) operation =
  let cwd = Global.ensure_supervision ?id ~body (module H) () in
  let file = Kohai_model.Resolver.transient_logs ~cwd in
  let now = Eff.now (module H) in
  let apply f = f ?id ~body (module H : Eff.HANDLER) file now in
  match operation with
  | M.Record { date_query; project; sector; label } ->
    apply action_record date_query project sector label
  | M.Stop_recording { index; duration } ->
    apply stop_recording_action index duration
  | M.Rewrite { index; date_query; project; sector; label } ->
    apply rewrite_action index date_query project sector label
  | M.Delete { index } -> apply delete_action index
  | M.Add_meta { index; key; value } -> apply add_meta_action index key value
  | M.Remove_meta { index; key } -> apply remove_meta_action index key
  | M.Add_link { index; key; value } -> apply add_link_action index key value
  | M.Remove_link { index; key } -> apply remove_link_action index key
  | M.Promote { index } -> apply promote_action cwd index
;;
