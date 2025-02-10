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
