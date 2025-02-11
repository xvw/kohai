module Recored = struct
  type t =
    { start_date : Datetime.t option
    ; project : string option
    ; sector : string
    ; label : string
    }

  let start_date_of { start_date; _ } = start_date
  let sector_of { sector; _ } = sector

  let from_rensai =
    let open Rensai.Validation in
    record (fun obj ->
      let open Record in
      let+ start_date = optional obj "start_date" Datetime.from_rensai
      and+ project = optional obj "project" string
      and+ sector = required obj "sector" (string & String.is_slug)
      and+ label = required obj "label" (string & String.is_not_blank) in
      { start_date; project; sector; label })
  ;;
end

module Transient = struct
  type t =
    { index : int
    ; start_date : Datetime.t
    ; duration : int64 option
    ; project : string option
    ; sectors : string list
    ; label : string
    }

  let make ~start_date ({ project; sector; label; _ } : Recored.t) =
    { index = -1
    ; start_date
    ; duration = None
    ; project
    ; sectors = [ sector ]
    ; label
    }
  ;;

  let to_rensai { index; start_date; duration; project; sectors; label } =
    let open Rensai.Ast in
    record
      [ "index", int index
      ; "start_date", Datetime.to_compact_rensai start_date
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
      and+ index = optional_or ~default:(-1) obj "index" int
      and+ duration = optional obj "duration" int64
      and+ project = optional obj "project" string
      and+ sectors =
        optional_or
          ~default:[]
          obj
          "sectors"
          (list_of (string & String.is_slug))
      and+ label = required obj "label" (string & String.is_not_blank) in
      { index; start_date; duration; project; sectors; label })
  ;;

  let compute_duration log dt =
    match log.duration with
    | None when Datetime.Infix.(log.start_date < dt) ->
      let d = Datetime.diff log.start_date dt in
      { log with duration = Some d }
    | Some _ | None -> log
  ;;

  let index list =
    list
    |> List.sort (fun { start_date = a; _ } { start_date = b; _ } ->
      Datetime.compare a b)
    |> List.mapi (fun i log -> { log with index = i })
  ;;

  let push sector list = sector :: list |> index
end
