module R = Kohai_model.Resolver
module L = Kohai_model.Log
module S = Kohai_model.State

let upgrade_state (module H : Eff.HANDLER) cwd log =
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
  |> S.increase_counter 1
  |> S.dump
  |> Eff.write_file (module H) state_file
;;

let downgrade_state (module H : Eff.HANDLER) cwd log =
  let duration = L.duration log in
  let state_file = R.state ~cwd in
  let content = Eff.read_file (module H) state_file in
  content
  |> S.from_string
  |> S.decrease_duration duration
  |> S.decrease_counter 1
  |> S.dump
  |> Eff.write_file (module H) state_file
;;

let upgrade_sector_state (module H : Eff.HANDLER) cwd log =
  let sector, _ = L.sector_and_project log in
  let _ = Sector.increase (module H) sector in
  (* Create state into sectors *)
  let sector_cwd = Path.(R.sector_folder ~cwd / sector) in
  upgrade_state (module H) sector_cwd log
;;

let downgrade_sector_state (module H : Eff.HANDLER) cwd log =
  let sector, _ = L.sector_and_project log in
  let _ = Sector.decrease (module H) sector in
  (* Create state into sectors *)
  let sector_cwd = Path.(R.sector_folder ~cwd / sector) in
  downgrade_state (module H) sector_cwd log
;;

let upgrade_project_state (module H : Eff.HANDLER) cwd log =
  let _, project = L.sector_and_project log in
  project
  |> Option.iter (fun project ->
    let _ = Project.increase (module H) project in
    let project_cwd = Path.(R.project_folder ~cwd / project) in
    upgrade_state (module H) project_cwd log)
;;

let downgrade_project_state (module H : Eff.HANDLER) cwd log =
  let _, project = L.sector_and_project log in
  project
  |> Option.iter (fun project ->
    let _ = Project.decrease (module H) project in
    let project_cwd = Path.(R.project_folder ~cwd / project) in
    downgrade_state (module H) project_cwd log)
;;

let upgrade (module H : Eff.HANDLER) cwd log =
  let () = upgrade_state (module H) cwd log in
  let () = upgrade_sector_state (module H) cwd log in
  upgrade_project_state (module H) cwd log
;;

let downgrade (module H : Eff.HANDLER) cwd log =
  let () = downgrade_state (module H) cwd log in
  let () = downgrade_sector_state (module H) cwd log in
  downgrade_project_state (module H) cwd log
;;

let get_by_cwd (module H : Eff.HANDLER) cwd =
  let file = R.state ~cwd in
  let content = Eff.read_file (module H) file in
  content |> S.from_string
;;

let get (module H : Eff.HANDLER) () =
  let cwd = Global.ensure_supervision (module H) () in
  get_by_cwd (module H) cwd
;;

let get_for_sector (module H : Eff.HANDLER) sector =
  let cwd = Global.ensure_supervision (module H) () in
  let sector = Path.(R.sector_folder ~cwd / sector) in
  get_by_cwd (module H) sector
;;

let get_for_project (module H : Eff.HANDLER) project =
  let cwd = Global.ensure_supervision (module H) () in
  let project = Path.(R.project_folder ~cwd / project) in
  get_by_cwd (module H) project
;;
