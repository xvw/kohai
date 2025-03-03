type t =
  { start_date : Datetime.t
  ; duration : int
  ; project : string option
  ; sector : string
  ; label : string
  ; meta : string Key_value.t
  ; links : Url.t Key_value.t
  ; id : Uuid.t
  }

let id { id; _ } = id
let sector_and_project { sector; project; _ } = sector, project
let start_date { start_date; _ } = start_date
let end_date { start_date; duration; _ } = Datetime.(start_date + sec duration)
let duration { duration; _ } = duration

let from_transient_log tl =
  tl
  |> Transient_log.duration
  |> Option.map (fun duration ->
    let start_date = Transient_log.start_date tl
    and project = Transient_log.project tl
    and sector = Transient_log.sector tl
    and label = Transient_log.label tl
    and meta = Transient_log.meta tl
    and links = Transient_log.links tl
    and id = Uuid.gen (Transient_log.string_repr tl) in
    { start_date; duration; project; sector; label; meta; links; id })
;;

let to_transient_log { start_date; project; sector; label; meta; links; _ } =
  Transient_log.make ~meta ~links ~start_date ~project ~sector ~label ()
;;

let slug = Rensai.Validation.(string & String.is_non_empty_slug)
let positive_int = Rensai.Validation.(int & Int.is_positive)

let from_rensai =
  let open Rensai.Validation in
  record (fun b ->
    let open Record in
    let+ start_date = required b "start_date" Datetime.from_rensai
    and+ id = required b "id" Uuid.from_rensai
    and+ project = optional b "project" slug
    and+ duration = required b "duration" positive_int
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
    { start_date; duration; project; sector; label; meta; links; id })
;;

let to_rensai_record
      { start_date; duration; project; sector; label; meta; links; id }
  =
  let open Rensai.Ast in
  [ "start_date", Datetime.to_compact_rensai start_date
  ; "id", Uuid.to_rensai id
  ; "duration", int duration
  ; "project", option string project
  ; "sector", string sector
  ; "label", string label
  ; "meta", Key_value.to_rensai string meta
  ; "links", Key_value.to_rensai Url.to_compact_rensai links
  ]
;;

let to_rensai log = log |> to_rensai_record |> Rensai.Ast.record

let duration_repr duration =
  duration
  |> Int64.of_int
  |> Datetime.seconds_to_duration
  |> Format.asprintf "%a" Datetime.pp_duration
  |> Rensai.Ast.string
;;

let return_rensai (now, ({ start_date; duration; _ } as log)) =
  let open Rensai.Ast in
  record
    (("duration_repr", duration_repr duration)
     :: ( "start_date_repr"
        , string (Format.asprintf "%a" (Datetime.pp_relative now) start_date) )
     :: to_rensai_record log)
;;

let list_to_rensai (now, logs) =
  Rensai.Ast.list (fun x -> return_rensai (now, x)) logs
;;

let from_file_content content =
  let lexbuf = Lexing.from_string content in
  lexbuf |> Rensai.Lang.from_lexingbuf_or_null |> from_rensai
;;

let find_file_by_month ~cwd { start_date; _ } =
  Datetime.as_month_file ~ext:"rens" ~cwd start_date
;;

let find_file ~cwd { id; _ } =
  let fragment = Uuid.to_string id ^ ".rens" in
  Path.(cwd / fragment)
;;

let ord_log a b =
  let c = Datetime.compare b.start_date a.start_date in
  if Int.equal 0 c then Int.compare b.duration a.duration else c
;;

let sort list = list |> List.sort ord_log

let truncate_list ?(len = 50) log list =
  log :: list |> sort |> List.take len |> List.map id |> Uuid.Set.from_list
;;
