module R = Kohai_model.Resolver
module L = Kohai_model.Log

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

let promote ?body ?id (module H : Eff.HANDLER) cwd log =
  let log_file = L.find_file ~cwd:(R.all_logs ~cwd) log in
  let content = Rensai.Lang.dump L.to_rensai log in
  let () = Eff.write_file (module H) log_file content in
  let () = propagate (module H) cwd log in
  State.update ?body ?id (module H) cwd log
;;

let get ?body ?id (module H : Eff.HANDLER) uuid =
  let uuid = Uuid.to_string uuid in
  let cwd = Global.ensure_supervision ?body ?id (module H) () in
  let file = Path.(R.all_logs ~cwd / (uuid ^ ".rens")) in
  file |> Eff.read_file (module H) |> L.from_file_content |> Result.to_option
;;
