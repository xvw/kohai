module Transient = struct
  type t =
    { start_date : Datetime.t
    ; duration : int64 option
    ; project : string option
    ; sectors : string list
    ; label : string
    }

  let to_rensai { start_date; duration; project; sectors; label } =
    let open Rensai.Ast in
    record
      [ "start_date", Datetime.to_rensai start_date
      ; "duration", option int64 duration
      ; "project", option string project
      ; "sectors", list string sectors
      ; "label", string label
      ]
  ;;

  let from_rensai =
    let open Rensai.Validation in
    record (fun obj ->
      let open Record in
      let+ start_date = required obj "start_date" Datetime.from_rensai
      and+ duration = optional obj "duration" int64
      and+ project = optional obj "project" string
      and+ sectors =
        optional_or
          ~default:[]
          obj
          "sectors"
          (list_of (string & String.is_slug))
      and+ label = required obj "label" (string & String.is_not_blank) in
      { start_date; duration; project; sectors; label })
  ;;

  let compute_duration log dt =
    match log.duration with
    | None when Datetime.Infix.(log.start_date < dt) ->
      let d = Datetime.diff log.start_date dt in
      { log with duration = Some d }
    | Some _ | None -> log
  ;;
end
