type operation =
  | Record of
      { start_date : Datetime.t option
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
      ; start_date : Datetime.t option
      ; project : string option
      ; sector : string
      ; label : string
      }

type t =
  { index : int
  ; start_date : Datetime.t
  ; duration : int option
  ; project : string option
  ; sector : string
  ; label : string
  }

type result =
  { inserted : t option
  ; outdated : t list
  ; all : t list
  }

let has_index i { index; _ } = Int.equal i index

let make ~start_date ~project ~sector ~label =
  { index = -1; start_date; duration = None; project; sector; label }
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
    and+ duration = optional b "duration" int
    and+ sector = required b "sector" slug
    and+ label = required b "label" (string & String.is_not_blank) in
    { index; start_date; duration; project; sector; label })
;;

let to_rensai { index; start_date; duration; project; sector; label } =
  let open Rensai.Ast in
  record
    [ "index", int index
    ; "start_date", Datetime.to_compact_rensai start_date
    ; "duration", option int duration
    ; "project", option string project
    ; "sector", string sector
    ; "label", string label
    ]
;;

let result_to_rensai { inserted; outdated; all } =
  let open Rensai.Ast in
  record
    [ "inserted", option to_rensai inserted
    ; "outdated", list to_rensai outdated
    ; "all", list to_rensai all
    ]
;;

let operation_from_rensai =
  let open Rensai.Validation in
  sum
    [ ( "record"
      , record (fun b ->
          let open Record in
          let+ start_date = optional b "start_date" Datetime.from_rensai
          and+ project = optional b "project" slug
          and+ sector = required b "sector" slug
          and+ label = required b "label" (string & String.is_not_blank) in
          Record { start_date; project; sector; label }) )
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
          and+ start_date = optional b "start_date" Datetime.from_rensai
          and+ project = optional b "project" slug
          and+ sector = required b "sector" slug
          and+ label = required b "label" (string & String.is_not_blank) in
          Rewrite { index; start_date; project; sector; label }) )
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
      |> List.filter (fun log ->
        match log.duration with
        | Some _ -> false
        | None -> Datetime.Infix.(inserted.start_date > log.start_date))
      |> sort
    in
    { all; inserted = Some inserted; outdated }
;;

let dump { all; _ } = Rensai.Lang.dump_list to_rensai all

let use_static_duration duration log =
  match log.duration with
  | None ->
    let duration = Some (duration * 60) in
    { log with duration }
  | Some _ -> log
;;

let use_datetime dt log =
  match log.duration with
  | None ->
    let duration =
      dt |> Datetime.diff log.start_date |> Int64.to_int |> Option.some
    in
    { log with duration }
  | Some _ -> log
;;

let finalize_duration dt log = function
  | None -> use_datetime dt log
  | Some d -> use_static_duration d log
;;
