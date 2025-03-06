let check_supervised_path ?id ~body (module H : Eff.HANDLER) path =
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
  else path
;;

let ensure_supervision ?id ~body (module H : Eff.HANDLER) () =
  match Eff.get_supervised_directory (module H) with
  | None -> Eff.raise (module H) @@ Error.no_supervised_directory ?id ~body ()
  | Some path ->
    let _ = check_supervised_path ?id ~body (module H) path in
    path
;;

let with_supervision ?id ~body (module H : Eff.HANDLER) callback arg =
  let _ = ensure_supervision ~body ?id (module H) () in
  callback ?id ~body (module H : Eff.HANDLER) arg
;;
