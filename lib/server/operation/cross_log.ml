let unpromote_log ?body ?id (module H : Eff.HANDLER) uuid =
  let cwd = Global.ensure_supervision ?body ?id (module H) () in
  let now = Eff.now (module H) in
  let file = Kohai_model.Resolver.transient_logs ~cwd in
  let () =
    Option.iter
      (fun (_, log) ->
         let log_file =
           Kohai_model.Log.find_file
             ~cwd:(Kohai_model.Resolver.all_logs ~cwd)
             log
         in
         let transient = Kohai_model.Log.to_transient_log log in
         let () = Log.unpromote ?body ?id (module H) cwd log in
         let _ = Transient_log.save ?body ?id (module H) file now transient in
         Eff.delete (module H) log_file)
      (Log.get ?body ?id (module H) uuid)
  in
  Transient_log.list ?body ?id (module H : Eff.HANDLER) ()
;;
