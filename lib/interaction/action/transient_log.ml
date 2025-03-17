module TL = Kohai_model.Transient_log

let all (module H : Eff.HANDLER) =
  let cwd = Global.ensure_supervision (module H) () in
  let file = Kohai_model.Resolver.transient_logs ~cwd in
  let content = Eff.read_file (module H) file in
  TL.from_file_content content
;;
