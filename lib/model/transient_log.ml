type operation =
  | Record of
      { date_query : Datetime.Query.t option
      ; project : string option
      ; sector : string
      ; label : string
      }
  | Stop_recording of
      { index : int
      ; duration : int option
      }
  | Rewrite of
      { index : int
      ; date_query : Datetime.Query.t option
      ; project : string option
      ; sector : string
      ; label : string
      }
  | Delete of { index : int }
  | Add_meta of
      { index : int
      ; key : string
      ; value : string
      }
  | Remove_meta of
      { index : int
      ; key : string
      }
  | Add_link of
      { index : int
      ; key : string
      ; value : Url.t
      }
  | Remove_link of
      { index : int
      ; key : string
      }
  | Promote of { index : int }
  | Duplicate of { index : int }

let action_delete index = Delete { index }

type t =
  { index : int
  ; start_date : Datetime.t
  ; duration : Duration.t option
  ; project : string option
  ; sector : string
  ; label : string
  ; meta : string Key_value.t
  ; links : Url.t Key_value.t
  }

type result =
  { inserted : t option
  ; outdated : (t * int) list
  ; all : t list
  }

let has_index i { index; _ } = Int.equal i index

let make
      ?(meta = Key_value.empty ())
      ?(links = Key_value.empty ())
      ?duration
      ~start_date
      ~project
      ~sector
      ~label
      ()
  =
  let duration = Option.map Duration.from_int duration in
  { index = -1; start_date; duration; project; sector; label; meta; links }
;;

let slug = Rensai.Validation.(string & String.is_non_empty_slug)
let positive_int = Rensai.Validation.(int & Int.is_positive)

let from_rensai =
  let open Rensai.Validation in
  record (fun b ->
    let open Record in
    let+ start_date = required b "start_date" Datetime.from_rensai
    and+ index = optional_or ~default:(-1) b "index" int
    and+ project = optional b "project" slug
    and+ duration = optional b "duration" (int $ Duration.from_int)
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
    { index; start_date; duration; project; sector; label; meta; links })
;;

let duration_repr duration =
  duration |> Format.asprintf "%a" Duration.pp |> Rensai.Ast.string
;;

let to_rensai_record
      { index; start_date; duration; project; sector; label; meta; links }
  =
  let open Rensai.Ast in
  [ "index", int index
  ; "start_date", Datetime.to_compact_rensai start_date
  ; "duration", option Duration.to_rensai duration
  ; "project", option string project
  ; "sector", string sector
  ; "label", string label
  ; "meta", Key_value.to_rensai string meta
  ; "links", Key_value.to_rensai Url.to_compact_rensai links
  ]
;;

let to_rensai log = log |> to_rensai_record |> Rensai.Ast.record

let return_rensai (now, ({ start_date; duration; _ } as log)) =
  let open Rensai.Ast in
  record
    (("duration_repr", option duration_repr duration)
     :: ( "start_date_repr"
        , string (Format.asprintf "%a" (Datetime.pp_relative now) start_date) )
     :: to_rensai_record log)
;;

let list_to_rensai (now, logs) =
  Rensai.Ast.list (fun x -> return_rensai (now, x)) logs
;;

let result_to_rensai (now, { inserted; outdated; all }) =
  let open Rensai.Ast in
  record
    [ "inserted", option to_rensai inserted
    ; ( "outdated"
      , list
          (fun (r, d) ->
             record
               [ "record", return_rensai (now, r); "computed_duration", int d ])
          outdated )
    ; "all", list_to_rensai (now, all)
    ]
;;

let adding_from_rensai validator f =
  let open Rensai.Validation in
  record (fun b ->
    let open Record in
    let+ index = required b "index" positive_int
    and+ key = required b "key" (string & String.is_not_blank)
    and+ value =
      required b "value" (string & String.is_not_blank & validator)
    in
    f index key value)
;;

let removing_from_rensai f =
  let open Rensai.Validation in
  record (fun b ->
    let open Record in
    let+ index = required b "index" positive_int
    and+ key = required b "key" (string & String.is_not_blank) in
    f index key)
;;

let operation_from_rensai =
  let open Rensai.Validation in
  sum
    [ ( "record"
      , record (fun b ->
          let open Record in
          let+ date_query = optional b "date_query" Datetime.Query.from_rensai
          and+ project = optional b "project" slug
          and+ sector = required b "sector" slug
          and+ label = required b "label" (string & String.is_not_blank) in
          Record { date_query; project; sector; label }) )
    ; ( "stop_recording"
      , record (fun b ->
          let open Record in
          let+ index = required b "index" positive_int
          and+ duration = optional b "duration" positive_int in
          Stop_recording { index; duration }) )
    ; ( "rewrite"
      , record (fun b ->
          let open Record in
          let+ index = required b "index" positive_int
          and+ date_query = optional b "date_query" Datetime.Query.from_rensai
          and+ project = optional b "project" slug
          and+ sector = required b "sector" slug
          and+ label = required b "label" (string & String.is_not_blank) in
          Rewrite { index; date_query; project; sector; label }) )
    ; ( "delete"
      , record (fun b ->
          let open Record in
          let+ index = required b "index" positive_int in
          Delete { index }) )
    ; ( "add_meta"
      , adding_from_rensai
          (fun x -> Ok x)
          (fun index key value -> Add_meta { index; key; value }) )
    ; ( "remove_meta"
      , removing_from_rensai (fun index key -> Remove_meta { index; key }) )
    ; ( "add_link"
      , adding_from_rensai Url.from_string (fun index key value ->
          Add_link { index; key; value }) )
    ; ( "remove_link"
      , removing_from_rensai (fun index key -> Remove_link { index; key }) )
    ; ( "promote"
      , record (fun b ->
          let open Record in
          let+ index = required b "index" positive_int in
          Promote { index }) )
    ; ( "duplicate"
      , record (fun b ->
          let open Record in
          let+ index = required b "index" positive_int in
          Duplicate { index }) )
    ]
;;

let from_file_content content =
  let lexbuf = Lexing.from_string content in
  lexbuf
  |> Rensai.Lang.from_lexingbuf_to_list ~reverse:false
  |> List.filter_map (fun x -> x |> from_rensai |> Result.to_option)
;;

let compare { start_date = a; _ } { start_date = b; _ } = Datetime.compare a b

let sort list =
  list |> List.sort compare |> List.mapi (fun index log -> { log with index })
;;

let to_result ?inserted all =
  match inserted with
  | None -> { inserted; all = sort all; outdated = [] }
  | Some inserted ->
    let all = sort (inserted :: all) in
    let outdated =
      all
      |> List.filter_map (fun log ->
        match log.duration with
        | Some _ -> None
        | None ->
          if Datetime.Infix.(inserted.start_date > log.start_date)
          then (
            let d =
              Datetime.diff inserted.start_date log.start_date
              |> Duration.to_int
              |> fun x -> x / 60
            in
            Some (log, d))
          else None)
    in
    { all; inserted = Some inserted; outdated }
;;

let dump { all; _ } = Rensai.Lang.dump_list to_rensai all

let use_static_duration duration log =
  match log.duration with
  | None ->
    let duration = Some (Duration.from_int @@ (duration * 60)) in
    { log with duration }
  | Some _ -> log
;;

let use_datetime dt log =
  match log.duration with
  | None ->
    let duration = dt |> Datetime.diff log.start_date |> Option.some in
    { log with duration }
  | Some _ -> log
;;

let finalize_duration dt log = function
  | None -> use_datetime dt log
  | Some d -> use_static_duration d log
;;

let add_meta ~key ~value log =
  { log with meta = Key_value.add key value log.meta }
;;

let add_link ~key ~value log =
  { log with links = Key_value.add key value log.links }
;;

let remove_meta ~key log = { log with meta = Key_value.remove key log.meta }
let remove_link ~key log = { log with links = Key_value.remove key log.links }
let start_date { start_date; _ } = start_date
let duration { duration; _ } = duration
let project { project; _ } = project
let sector { sector; _ } = sector
let label { label; _ } = label
let meta { meta; _ } = meta
let links { links; _ } = links

let string_repr { start_date; duration; project; sector; label; _ } =
  Format.asprintf
    "%a-%a-%a-%s-%s"
    (Datetime.pp_rfc822 ())
    start_date
    (Format.pp_print_option Format.pp_print_int)
    (Option.map Duration.to_int duration)
    (Format.pp_print_option Format.pp_print_string)
    project
    sector
    label
;;

module Expanded = struct
  let as_list ctx list =
    let now = Context.now ctx in
    list_to_rensai (now, list)
  ;;

  let as_result ctx result =
    let now = Context.now ctx in
    result_to_rensai (now, result)
  ;;
end

let duplicate log =
  { log with start_date = Datetime.succ_day log.start_date; duration = None }
;;
