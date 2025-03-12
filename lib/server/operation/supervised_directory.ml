let set (module H : Eff.HANDLER) path =
  let path = Global.check_supervised_path (module H) path in
  let () = Eff.set_supervised_directory (module H) (Some path) in
  path
;;

let get (module H : Eff.HANDLER) () = Eff.get_supervised_directory (module H)

let is_valid (module H : Eff.HANDLER) path =
  Path.is_absolute path && Eff.is_dir (module H) path
;;
