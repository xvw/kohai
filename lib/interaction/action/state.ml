module S = Kohai_model.State
module L = Kohai_model.Log

let update_state f dir (module H : Eff.HANDLER) log =
  let state_file = Kohai_model.Resolver.state ~cwd:dir in
  state_file
  |> Eff.read_file (module H)
  |> S.from_string
  |> f log
  |> S.dump
  |> Eff.write_file (module H) state_file
;;

let upgrade_state =
  update_state (fun log state ->
    let start_date = L.start_date log
    and end_date = L.end_date log
    and duration = L.duration log in
    state
    |> S.patch_date_boundaries start_date
    |> S.patch_date_boundaries end_date
    |> S.increase_duration duration
    |> S.increase_counter 1)
;;

let downgrade_state =
  update_state (fun log state ->
    let duration = L.duration log in
    state |> S.decrease_duration duration |> S.decrease_counter 1)
;;

let update_particular_state
      on_subject
      resolver
      upgrade
      increment
      dir
      (module H : Eff.HANDLER)
      log
  =
  match on_subject @@ L.sector_and_project log with
  | Some subject ->
    let _ = increment (module H : Eff.HANDLER) subject in
    let dir = Path.(resolver ~cwd:dir / subject) in
    upgrade dir (module H : Eff.HANDLER) log
  | None -> ()
;;

let update_sector_state =
  update_particular_state
    (fun (x, _) -> Some x)
    Kohai_model.Resolver.sector_folder
;;

let update_project_state =
  update_particular_state snd Kohai_model.Resolver.project_folder
;;

let upgrade_sector_state = update_sector_state upgrade_state Sector.increase
let downgrade_sector_state = update_sector_state downgrade_state Sector.decrease
let upgrade_project_state = update_project_state upgrade_state Project.increase

let downgrade_project_state =
  update_project_state downgrade_state Project.decrease
;;

let upgrade dir (module H : Eff.HANDLER) log =
  let () = upgrade_state dir (module H) log in
  let () = upgrade_sector_state dir (module H) log in
  upgrade_project_state dir (module H) log
;;

let downgrade dir (module H : Eff.HANDLER) log =
  let () = downgrade_state dir (module H) log in
  let () = downgrade_sector_state dir (module H) log in
  downgrade_project_state dir (module H) log
;;
