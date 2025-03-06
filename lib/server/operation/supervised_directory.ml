let set ?id ~body (module H : Eff.HANDLER) path =
  let path = Global.check_supervised_path ~body ?id (module H) path in
  let () = Eff.set_supervised_directory (module H) (Some path) in
  path
;;

let get ?id:_ ~body:_ (module H : Eff.HANDLER) () =
  Eff.get_supervised_directory (module H)
;;

let is_valid ?id:_ ~body:_ (module H : Eff.HANDLER) path =
  Path.is_absolute path && Eff.is_dir (module H) path
;;
