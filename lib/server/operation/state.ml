module R = Kohai_model.Resolver
module L = Kohai_model.Log
module S = Kohai_model.State

let update_state (module H : Eff.HANDLER) cwd log =
  let start_date = L.start_date log
  and end_date = L.end_date log
  and duration = L.duration log in
  let state_file = R.state ~cwd in
  let content = Eff.read_file (module H) state_file in
  content
  |> S.from_string
  |> S.patch_date_boundaries start_date
  |> S.patch_date_boundaries end_date
  |> S.increase_duration duration
  |> S.dump
  |> Eff.write_file (module H) state_file
;;

let update_sector_state ?body ?id (module H : Eff.HANDLER) cwd log =
  let sector, _ = L.sector_and_project log in
  let _ = Sector.increase ?body ?id (module H) sector in
  (* Create state into sectors *)
  let sector_cwd = Path.(R.sector_folder ~cwd / sector) in
  update_state (module H) sector_cwd log
;;

let update_project_state ?body ?id (module H : Eff.HANDLER) cwd log =
  let _, project = L.sector_and_project log in
  project
  |> Option.iter (fun project ->
    let _ = Project.increase ?body ?id (module H) project in
    let project_cwd = Path.(R.project_folder ~cwd / project) in
    update_state (module H) project_cwd log)
;;

let update ?body ?id (module H : Eff.HANDLER) cwd log =
  let () = update_state (module H) cwd log in
  let () = update_sector_state ?body ?id (module H) cwd log in
  update_project_state ?body ?id (module H) cwd log
;;

let get ?body ?id (module H : Eff.HANDLER) () =
  let cwd = Global.ensure_supervision ?body ?id (module H) () in
  let file = R.state ~cwd in
  let content = Eff.read_file (module H) file in
  content |> S.from_string
;;
