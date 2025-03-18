let check_supervised_path (module H : Eff.HANDLER) path =
  if Path.is_relative path
  then
    Eff.raise (module H)
    @@ Error.supervised_directory_error
         ~message:"Supervised directory need to be absolute"
         ()
  else if not (Eff.is_dir (module H) path)
  then
    Eff.raise (module H)
    @@ Error.supervised_directory_error
         ~message:"The given directory does not exists"
         ()
  else path
;;

let ensure_supervision (module H : Eff.HANDLER) () =
  match Eff.get_supervised_directory (module H) with
  | None -> Eff.raise (module H) @@ Error.no_supervised_directory ()
  | Some path ->
    let _ = check_supervised_path (module H) path in
    path
;;

let with_supervision (module H : Eff.HANDLER) callback arg =
  let _ = ensure_supervision (module H) () in
  callback (module H : Eff.HANDLER) arg
;;
