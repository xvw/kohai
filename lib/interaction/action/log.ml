module L = Kohai_model.Log

let get_by_uuid (module H : Eff.HANDLER) dir uuid =
  let uuid = Uuid.to_string uuid in
  let file = Path.(Kohai_model.Resolver.all_logs ~cwd:dir / (uuid ^ ".rens")) in
  file |> Eff.read_file (module H) |> L.from_file_content |> Result.to_option
;;

let update_last_list f dir (module H : Eff.HANDLER) log =
  let log_file = Kohai_model.Resolver.last_logs ~cwd:dir in
  let set =
    log_file
    |> Eff.read_file (module H)
    |> Uuid.Set.from_file_content
    |> f dir log
    |> Uuid.Set.dump
  in
  Eff.write_file (module H) log_file set
;;

let propagate_last_list dir (module H : Eff.HANDLER) =
  update_last_list
    (fun dir log set ->
       set
       |> Uuid.Set.to_list
       |> List.filter_map (get_by_uuid (module H) dir)
       |> L.truncate_list log)
    dir
    (module H)
;;

let unpropagate_last_list =
  update_last_list (fun _dir log set ->
    let id = L.id log in
    set |> Uuid.Set.remove id)
;;

let update_set f dir (module H : Eff.HANDLER) log =
  let folder = Kohai_model.Resolver.logs ~cwd:dir in
  let log_id = L.id log in
  let file = L.find_file_by_month ~cwd:folder log in
  let set =
    file
    |> Eff.read_file (module H)
    |> Uuid.Set.from_file_content
    |> f ~id:log_id
  in
  let content = Uuid.Set.dump set in
  Eff.write_file (module H) file content
;;

let unpropagate_from = update_set (fun ~id -> Uuid.Set.remove id)
let propagate_into = update_set (fun ~id -> Uuid.Set.push id)

let update_propagation f dir (module H : Eff.HANDLER) log =
  let sector, project = L.sector_and_project log in
  [ f dir; f Path.(Kohai_model.Resolver.sector_folder ~cwd:dir / sector) ]
  @ List.map
      (fun project ->
         f Path.(Kohai_model.Resolver.project_folder ~cwd:dir / project))
      (Option.to_list project)
  |> List.iter (fun f -> f (module H : Eff.HANDLER) log)
;;

let make_propagation logs list state (module H : Eff.HANDLER) dir log =
  [ update_propagation logs; update_propagation list; state ]
  |> List.iter (fun f -> f dir (module H : Eff.HANDLER) log)
;;

let propagate =
  make_propagation propagate_into propagate_last_list State.upgrade
;;

let _unpropagate =
  make_propagation unpropagate_from unpropagate_last_list State.downgrade
;;

let promote (module H : Eff.HANDLER) transient_log =
  let cwd = Global.ensure_supervision (module H) () in
  transient_log
  |> L.from_transient_log
  |> Option.map (fun log ->
    let log_file = L.find_file ~cwd:(Kohai_model.Resolver.all_logs ~cwd) log in
    let content = Rensai.Lang.dump L.to_rensai log in
    let () = Eff.write_file (module H) log_file content in
    propagate (module H) cwd log)
;;
