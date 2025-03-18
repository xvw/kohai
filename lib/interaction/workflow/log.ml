let get = Action.Log.get
let last = Action.Log.last
let last_for_sector = Action.Log.last_for_sector
let last_for_project = Action.Log.last_for_project

let unpromote (module H : Eff.HANDLER) uuid =
  match Action.Log.unpromote (module H) uuid with
  | None -> Eff.raise (module H) @@ Error.no_related_log ~uuid ()
  | Some transient_log ->
    let cwd = Action.Global.ensure_supervision (module H) () in
    let file = Kohai_model.Resolver.transient_logs ~cwd in
    let _ = Action.Transient_log.save (module H) file transient_log in
    Action.Transient_log.list (module H) ()
;;
