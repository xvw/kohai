type t =
  { start_date : Datetime.t
  ; duration : int
  ; project : string option
  ; sector : string
  ; label : string
  ; meta : string Key_value.t
  ; links : Url.t Key_value.t
  }

let from_transient_log tl =
  tl
  |> Transient_log.duration
  |> Option.map (fun duration ->
    let start_date = Transient_log.start_date tl
    and project = Transient_log.project tl
    and sector = Transient_log.sector tl
    and label = Transient_log.label tl
    and meta = Transient_log.meta tl
    and links = Transient_log.links tl in
    { start_date; duration; project; sector; label; meta; links })
;;

let slug = Rensai.Validation.(string & String.is_non_empty_slug)
let positive_int = Rensai.Validation.(int & Int.is_positive)

let from_rensai =
  let open Rensai.Validation in
  record (fun b ->
    let open Record in
    let+ start_date = required b "start_date" Datetime.from_rensai
    and+ project = optional b "project" slug
    and+ duration = required b "duration" int
    and+ sector = required b "sector" slug
    and+ label = required b "label" (string & String.is_not_blank)
    and+ meta =
      optional_or
        ~default:(Key_value.empty ())
        b
        "meta"
        (Key_value.from_rensai string)
    and+ links =
      optional_or
        ~default:(Key_value.empty ())
        b
        "links"
        (Key_value.from_rensai Url.from_rensai)
    in
    { start_date; duration; project; sector; label; meta; links })
;;

let to_rensai_record
      { start_date; duration; project; sector; label; meta; links }
  =
  let open Rensai.Ast in
  [ "start_date", Datetime.to_compact_rensai start_date
  ; "duration", int duration
  ; "project", option string project
  ; "sector", string sector
  ; "label", string label
  ; "meta", Key_value.to_rensai string meta
  ; "links", Key_value.to_rensai Url.to_compact_rensai links
  ]
;;

let to_rensai log = log |> to_rensai_record |> Rensai.Ast.record

(* let resolve_file ~cwd { start_date; _ } = *)
(*   let year = start_date.Datetime.year in *)
(*   let month = start_date.Datetime.month |> Datetime.month_to_int in *)
(*   () *)
(* ;; *)
