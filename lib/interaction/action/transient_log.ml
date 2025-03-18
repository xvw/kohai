module TL = Kohai_model.Transient_log

let all (module H : Eff.HANDLER) =
  let cwd = Global.ensure_supervision (module H) () in
  let file = Kohai_model.Resolver.transient_logs ~cwd in
  let content = Eff.read_file (module H) file in
  TL.from_file_content content
;;

let get (module H : Eff.HANDLER) index =
  (module H) |> all |> List.find_opt (TL.has_index index)
;;

let store_missing_artifacts (module H : Eff.HANDLER) ~project ~sector =
  Sector.store_missing_key (module H) sector;
  Project.may_store_missing_key (module H) project
;;

let dump_transients (module H : Eff.HANDLER) ?transient transients file =
  let result = TL.to_result ?inserted:transient transients in
  let content = TL.dump result in
  let () = Eff.write_file (module H) file content in
  result
;;

let update_by_index (module H : Eff.HANDLER) ~index f =
  let cwd = Global.ensure_supervision (module H) () in
  let file = Kohai_model.Resolver.transient_logs ~cwd in
  let transients =
    (module H)
    |> all
    |> List.map (fun tl -> if TL.has_index index tl then f tl else tl)
  in
  dump_transients (module H) transients file
;;

let save (module H : Eff.HANDLER) file transient =
  let sector = TL.sector transient
  and project = TL.project transient in
  let () = store_missing_artifacts (module H) ~project ~sector in
  let transients = all (module H) in
  dump_transients (module H) ~transient transients file
;;

let record (module H : Eff.HANDLER) ctx ~date_query ~project ~sector ~label =
  let cwd = Global.ensure_supervision (module H) () in
  let file = Kohai_model.Resolver.transient_logs ~cwd in
  let now = Kohai_model.Context.now ctx in
  let start_date = Datetime.Query.resolve now date_query in
  let transient = TL.make ~start_date ~project ~sector ~label () in
  save (module H) file transient
;;

let rewrite
      (module H : Eff.HANDLER)
      ctx
      ~index
      ~date_query
      ~project
      ~sector
      ~label
  =
  let now = Kohai_model.Context.now ctx in
  let start_date = Datetime.Query.resolve now date_query in
  let transient = TL.make ~start_date ~project ~sector ~label () in
  update_by_index (module H) ~index (fun _ -> transient)
;;

let stop_record (module H : Eff.HANDLER) ctx ~index ~duration =
  let now = Kohai_model.Context.now ctx in
  update_by_index
    (module H)
    ~index
    (fun tl -> TL.finalize_duration now tl duration)
;;

let delete (module H : Eff.HANDLER) ~index =
  let cwd = Global.ensure_supervision (module H) () in
  let file = Kohai_model.Resolver.transient_logs ~cwd in
  let transients =
    (module H) |> all |> List.filter (fun tl -> not (TL.has_index index tl))
  in
  dump_transients (module H) transients file
;;

let add_kv f (module H : Eff.HANDLER) ~index ~key ~value =
  update_by_index (module H) ~index (f ~key ~value)
;;

let remove_kv f (module H : Eff.HANDLER) ~index ~key =
  update_by_index (module H) ~index (f ~key)
;;

let add_meta = add_kv TL.add_meta
let remove_meta = remove_kv TL.remove_meta
let add_link = add_kv TL.add_link
let remove_link = remove_kv TL.remove_link
