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

type t =
  { index : int
  ; start_date : Datetime.t
  ; duration : int option
  ; project : string option
  ; sector : string
  ; label : string
  }

type after_insertion =
  { inserted : t
  ; outdated : t list
  ; all : t list
  }

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
    and+ label = required b "labbel" (string & String.is_not_blank) in
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

let after_insertion_to_rensai { inserted; outdated; all } =
  let open Rensai.Ast in
  record
    [ "inserted", to_rensai inserted
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
          and+ label = required b "labbel" (string & String.is_not_blank) in
          Record { start_date; project; sector; label }) )
    ; ( "stop_recording"
      , record (fun b ->
          let open Record in
          let+ index = required b "index" positive_int
          and+ duration = optional b "duration" positive_int in
          Stop_recording { index; duration }) )
    ]
;;
