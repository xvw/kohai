module R = Kohai_model.Resolver
module L = Kohai_model.Log

let promote ?body ?id (module H : Eff.HANDLER) cwd log =
  let sector, project = L.sector_and_project log in
  let log_file = L.find_file ~cwd:(R.all_logs ~cwd) log in
  let content = Format.asprintf "%a" Rensai.Lang.pp (L.to_rensai log) in
  let () = Eff.write_file (module H) log_file content in
  let () = State.update_state (module H) cwd log in
  let _ = Sector.increase ?body ?id (module H) sector in
  let _ =
    match project with
    | None -> Project.list ?body ?id (module H) ()
    | Some p -> Project.increase ?body ?id (module H) p
  in
  ()
;;
