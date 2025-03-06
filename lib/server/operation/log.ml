module R = Kohai_model.Resolver
module L = Kohai_model.Log

let get_by_uuid (module H : Eff.HANDLER) cwd uuid =
  let uuid = Uuid.to_string uuid in
  let file = Path.(R.all_logs ~cwd / (uuid ^ ".rens")) in
  file |> Eff.read_file (module H) |> L.from_file_content |> Result.to_option
;;

let get ?id ~body (module H : Eff.HANDLER) uuid =
  let cwd = Global.ensure_supervision ?id ~body (module H) () in
  let now = Eff.now (module H) in
  get_by_uuid (module H) cwd uuid |> Option.map (fun x -> now, x)
;;

let compute_last_list cwd (module H : Eff.HANDLER) log =
  let file = R.last_logs ~cwd in
  let set =
    Eff.read_file (module H) file
    |> Uuid.Set.from_file_content
    |> Uuid.Set.to_list
    |> List.filter_map (get_by_uuid (module H) cwd)
    |> L.truncate_list log
  in
  let content = Uuid.Set.dump set in
  Eff.write_file (module H) file content
;;

let recompute_last_list cwd (module H : Eff.HANDLER) log =
  let file = R.last_logs ~cwd in
  let id = L.id log in
  let set =
    Eff.read_file (module H) file
    |> Uuid.Set.from_file_content
    |> Uuid.Set.remove id
  in
  let content = Uuid.Set.dump set in
  Eff.write_file (module H) file content
;;

let compute_last_cache (module H : Eff.HANDLER) cwd log =
  let sector, project = L.sector_and_project log in
  [ compute_last_list cwd
  ; compute_last_list Path.(R.sector_folder ~cwd / sector)
  ]
  @ List.map
      (fun project -> compute_last_list Path.(R.project_folder ~cwd / project))
      (Option.to_list project)
  |> List.iter (fun f -> f (module H : Eff.HANDLER) log)
;;

let recompute_last_cache (module H : Eff.HANDLER) cwd log =
  let sector, project = L.sector_and_project log in
  [ recompute_last_list cwd
  ; recompute_last_list Path.(R.sector_folder ~cwd / sector)
  ]
  @ List.map
      (fun project ->
         recompute_last_list Path.(R.project_folder ~cwd / project))
      (Option.to_list project)
  |> List.iter (fun f -> f (module H : Eff.HANDLER) log)
;;

let unpropagate_from (module H : Eff.HANDLER) cwd log =
  let folder = R.logs ~cwd in
  let file = L.find_file_by_month ~cwd:folder log in
  let id = L.id log in
  let set =
    Eff.read_file (module H) file
    |> Uuid.Set.from_file_content
    |> Uuid.Set.remove id
  in
  let content = Uuid.Set.dump set in
  Eff.write_file (module H) file content
;;

let propagate_into (module H : Eff.HANDLER) cwd log =
  let folder = R.logs ~cwd in
  let file = L.find_file_by_month ~cwd:folder log in
  let id = L.id log in
  let set =
    Eff.read_file (module H) file
    |> Uuid.Set.from_file_content
    |> Uuid.Set.push id
  in
  let content = Uuid.Set.dump set in
  Eff.write_file (module H) file content
;;

let propagate (module H : Eff.HANDLER) cwd log =
  let sector, project = L.sector_and_project log in
  [ propagate_into (module H) cwd
  ; propagate_into (module H) Path.(R.sector_folder ~cwd / sector)
  ; (fun log ->
      Option.iter
        (fun project ->
           propagate_into (module H) Path.(R.project_folder ~cwd / project) log)
        project)
  ]
  |> List.iter (fun f -> f log)
;;

let unpropagate (module H : Eff.HANDLER) cwd log =
  let sector, project = L.sector_and_project log in
  [ unpropagate_from (module H) cwd
  ; unpropagate_from (module H) Path.(R.sector_folder ~cwd / sector)
  ; (fun log ->
      Option.iter
        (fun project ->
           unpropagate_from
             (module H)
             Path.(R.project_folder ~cwd / project)
             log)
        project)
  ]
  |> List.iter (fun f -> f log)
;;

let promote ?id ~body (module H : Eff.HANDLER) cwd log =
  let log_file = L.find_file ~cwd:(R.all_logs ~cwd) log in
  let content = Rensai.Lang.dump L.to_rensai log in
  let () = Eff.write_file (module H) log_file content in
  let () = propagate (module H) cwd log in
  let () = compute_last_cache (module H) cwd log in
  State.upgrade ?id ~body (module H) cwd log
;;

let unpromote ?id ~body (module H : Eff.HANDLER) cwd log =
  let log_file = L.find_file ~cwd:(R.all_logs ~cwd) log in
  let content = Rensai.Lang.dump L.to_rensai log in
  let () = Eff.write_file (module H) log_file content in
  let () = unpropagate (module H) cwd log in
  let () = recompute_last_cache (module H) cwd log in
  State.downgrade ?id ~body (module H) cwd log
;;

let from_set ?id ~body (module H : Eff.HANDLER) set =
  let cwd = Global.ensure_supervision ?id ~body (module H) () in
  set |> Uuid.Set.to_list |> List.filter_map (get_by_uuid (module H) cwd)
;;

let get_list_by_cwd ?id ~body (module H : Eff.HANDLER) cwd =
  let now = Eff.now (module H) in
  let file = R.last_logs ~cwd in
  let content = Eff.read_file (module H) file in
  now, content |> Uuid.Set.from_file_content |> from_set ?id ~body (module H)
;;

let get_last ?id ~body (module H : Eff.HANDLER) () =
  let cwd = Global.ensure_supervision ?id ~body (module H) () in
  get_list_by_cwd ?id ~body (module H) cwd
;;

let get_last_for_sector ?id ~body (module H : Eff.HANDLER) sector =
  let cwd = Global.ensure_supervision ?id ~body (module H) () in
  let sector = Path.(R.sector_folder ~cwd / sector) in
  get_list_by_cwd ?id ~body (module H) sector
;;

let get_last_for_project ?id ~body (module H : Eff.HANDLER) project =
  let cwd = Global.ensure_supervision ?id ~body (module H) () in
  let sector = Path.(R.project_folder ~cwd / project) in
  get_list_by_cwd ?id ~body (module H) sector
;;
