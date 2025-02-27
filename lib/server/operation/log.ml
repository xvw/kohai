module R = Kohai_model.Resolver
module L = Kohai_model.Log

let promote ?body ?id (module H : Eff.HANDLER) cwd log =
  let log_file = L.find_file ~cwd:(R.all_logs ~cwd) log in
  let content = Format.asprintf "%a" Rensai.Lang.pp (L.to_rensai log) in
  let () = Eff.write_file (module H) log_file content in
  State.update ?body ?id (module H) cwd log
;;
