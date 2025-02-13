module M = Kohai_model.Transient_log

let all ?body ?id (module H : Eff.HANDLER) =
  let cwd = Global.ensure_supervision ?body ?id (module H) () in
  let file = Kohai_model.Resolver.transient_logs ~cwd in
  let content = Eff.read_file (module H) file in
  M.from_file_content content
;;

let list ?body ?id (module H : Eff.HANDLER) () =
  all ?body ?id (module H) |> M.sort
;;

let get ?body ?id (module H : Eff.HANDLER) index =
  all ?body ?id (module H) |> List.find_opt (M.has_index index)
;;

let store_missing_sector ?body ?id (module H : Eff.HANDLER) sector =
  sector |> Kohai_model.Sector.make |> Sector.save ?body ?id (module H)
;;

let action ?body ?id (module H : Eff.HANDLER) operation =
  let cwd = Global.ensure_supervision ?body ?id (module H) () in
  let file = Kohai_model.Resolver.transient_logs ~cwd in
  let now = Eff.now (module H) in
  match operation with
  | M.Record { start_date; project; sector; label } ->
    let _ =
      (* Store missing sectors *)
      sector |> store_missing_sector ?body ?id (module H)
    in
    let transients = all ?body ?id (module H) in
    let start_date = Option.value ~default:now start_date in
    let transient = M.make ~start_date ~project ~sector ~label in
    let result = M.to_result ~inserted:transient transients in
    let content = M.dump result in
    let () = Eff.write_file (module H) file content in
    result
  | M.Stop_recording { index; duration } ->
    let transients =
      all ?body ?id (module H)
      |> List.map (fun log ->
        if M.has_index index log
        then M.finalize_duration now log duration
        else log)
    in
    let result = M.to_result transients in
    let content = M.dump result in
    let () = Eff.write_file (module H) file content in
    result
  | M.Rewrite { index; start_date; project; sector; label } ->
    let _ =
      (* Store missing sectors *)
      sector |> store_missing_sector ?body ?id (module H)
    in
    let start_date = Option.value ~default:now start_date in
    let patched_log = M.make ~start_date ~project ~sector ~label in
    let transients =
      all ?body ?id (module H)
      |> List.map (fun log ->
        if M.has_index index log then patched_log else log)
    in
    let result = M.to_result ~inserted:patched_log transients in
    let content = M.dump result in
    let () = Eff.write_file (module H) file content in
    result
;;
