let ensure_supervision body ?id (module H : Eff.HANDLER) () =
  match Eff.get_supervised_directory (module H) with
  | None -> Eff.raise (module H) @@ Error.no_supervised_directory ?id ~body ()
  | Some x -> x
;;

let with_supervision body ?id (module H : Eff.HANDLER) callback arg =
  let _ = ensure_supervision body ?id (module H) () in
  callback body ?id (module H : Eff.HANDLER) arg
;;

let get_supervised_directory ?id:_ (module H : Eff.HANDLER) () =
  Eff.get_supervised_directory (module H)
;;

let is_valid_supervised_directory ?id:_ (module H : Eff.HANDLER) path =
  Path.is_absolute path && Eff.is_dir (module H) path
;;

let set_supervised_directory body ?id (module H : Eff.HANDLER) path =
  if Path.is_relative path
  then
    Eff.raise (module H)
    @@ Error.supervised_directory_error
         ~body
         ?id
         "Supervised directory need to be absolute"
         ()
  else if not (Eff.is_dir (module H) path)
  then
    Eff.raise (module H)
    @@ Error.supervised_directory_error
         ~body
         ?id
         "The given directory does not exists"
         ()
  else Eff.set_supervised_directory (module H) (Some path)
;;

let get_sectors body ?id (module H : Eff.HANDLER) () =
  let cwd = ensure_supervision body ?id (module H) () in
  let file = Kohai_model.Resolver.sectors ~cwd in
  let content = Eff.read_file (module H) file in
  let lexbuf = Lexing.from_string content in
  lexbuf
  |> Rensai.Lang.from_lexingbuf_to_list ~reverse:false
  |> Kohai_model.Sector.Set.from_list
;;

let save_sector body ?id (module H : Eff.HANDLER) sector =
  let cwd = ensure_supervision body ?id (module H) () in
  let file = Kohai_model.Resolver.sectors ~cwd in
  let sectors = get_sectors body ?id (module H : Eff.HANDLER) () in
  let sectors = Kohai_model.Sector.Set.push sector sectors in
  let content =
    sectors
    |> Kohai_model.Sector.Set.to_list
    |> List.map (fun sector ->
      Format.asprintf "%a" Rensai.Lang.pp (Kohai_model.Sector.to_rensai sector))
    |> String.concat "\n"
  in
  let () = Eff.write_file (module H) file content in
  sectors
;;

let get_transient_log' body ?id (module H : Eff.HANDLER) () =
  let cwd = ensure_supervision body ?id (module H) () in
  let file = Kohai_model.Resolver.transient_logs ~cwd in
  let content = Eff.read_file (module H) file in
  let lexbuf = Lexing.from_string content in
  lexbuf
  |> Rensai.Lang.from_lexingbuf_to_list ~reverse:false
  |> List.filter_map (fun x ->
    x |> Kohai_model.Log.Transient.from_rensai |> Result.to_option)
;;

let get_transient_log body ?id (module H : Eff.HANDLER) () =
  get_transient_log' body ?id (module H) () |> Kohai_model.Log.Transient.index
;;

let make_transient_log_content logs =
  logs
  |> List.map (fun sector ->
    Format.asprintf
      "%a"
      Rensai.Lang.pp
      (Kohai_model.Log.Transient.to_rensai sector))
  |> String.concat "\n"
;;

let record_log body ?id (module H : Eff.HANDLER) recorded =
  let cwd = ensure_supervision body ?id (module H) () in
  let _ =
    (* Store the sector if not exists. *)
    recorded
    |> Kohai_model.Log.Recored.sector_of
    |> Kohai_model.Sector.make
    |> save_sector body ?id (module H : Eff.HANDLER)
  in
  let logs = get_transient_log' body ?id (module H) () in
  let file = Kohai_model.Resolver.transient_logs ~cwd in
  let start_date =
    match Kohai_model.Log.Recored.start_date_of recorded with
    | Some dt -> dt
    | None -> Eff.now (module H)
  in
  let log = Kohai_model.Log.Transient.make ~start_date recorded in
  let logs = Kohai_model.Log.Transient.push log logs in
  let content = make_transient_log_content logs in
  let () = Eff.write_file (module H) file content in
  logs |> Kohai_model.Log.Transient.split log
;;

let stop_recording body ?id (module H : Eff.HANDLER) operation =
  let cwd = ensure_supervision body ?id (module H) () in
  let logs = get_transient_log' body ?id (module H) () in
  let file = Kohai_model.Resolver.transient_logs ~cwd in
  let i = Kohai_model.Log.Transient.Operate.Stop.index operation in
  let logs =
    logs
    |> List.map (fun log ->
      let j = Kohai_model.Log.Transient.index_of log in
      let d = Kohai_model.Log.Transient.Operate.Stop.duration operation in
      if Int.equal i j
      then (
        match d with
        | None ->
          Kohai_model.Log.Transient.compute_duration log (Eff.now (module H))
        | Some d -> Kohai_model.Log.Transient.push_duration log d)
      else log)
    |> Kohai_model.Log.Transient.index
  in
  let content = make_transient_log_content logs in
  let () = Eff.write_file (module H) file content in
  logs
;;

let rewrite_transient_log body ?id (module H : Eff.HANDLER) current =
  let cwd = ensure_supervision body ?id (module H) () in
  let _ =
    (* Store the sector if not exists. *)
    current
    |> Kohai_model.Log.Transient.sector_of
    |> Kohai_model.Sector.make
    |> save_sector body ?id (module H : Eff.HANDLER)
  in
  let logs = get_transient_log' body ?id (module H) () in
  let file = Kohai_model.Resolver.transient_logs ~cwd in
  let i = Kohai_model.Log.Transient.index_of current in
  let logs =
    logs
    |> List.map (fun log ->
      let j = Kohai_model.Log.Transient.index_of log in
      if Int.equal i j then current else log)
    |> Kohai_model.Log.Transient.index
  in
  let content = make_transient_log_content logs in
  let () = Eff.write_file (module H) file content in
  logs
;;
