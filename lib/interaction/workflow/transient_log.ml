let list = Action.Transient_log.list
let get = Action.Transient_log.get

let action (module H : Eff.HANDLER) ctx = function
  | Kohai_model.Transient_log.Record { date_query; project; sector; label } ->
    Action.Transient_log.record
      (module H)
      ctx
      ~date_query
      ~project
      ~sector
      ~label
  | Stop_recording { index; duration } ->
    Action.Transient_log.stop_record (module H) ctx ~index ~duration
  | Rewrite { index; date_query; project; sector; label } ->
    Action.Transient_log.rewrite
      (module H)
      ctx
      ~index
      ~date_query
      ~project
      ~sector
      ~label
  | Delete { index } -> Action.Transient_log.delete (module H) ~index
  | Add_meta { index; key; value } ->
    Action.Transient_log.add_meta (module H) ~index ~key ~value
  | Remove_meta { index; key } ->
    Action.Transient_log.remove_meta (module H) ~index ~key
  | Add_link { index; key; value } ->
    Action.Transient_log.add_link (module H) ~index ~key ~value
  | Remove_link { index; key } ->
    Action.Transient_log.remove_link (module H) ~index ~key
  | Promote { index } ->
    (match
       Option.bind
         (Action.Transient_log.get (module H) index)
         (Action.Log.promote (module H))
     with
     | Some () -> Action.Transient_log.delete (module H) ~index
     | None -> Eff.raise (module H) (Error.no_related_transient_log ~index ()))
;;
