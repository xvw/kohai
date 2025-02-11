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
    ; duration : int option
    ; project : string option
    ; sectors : string list
    ; label : string
    }

  type result = t * t list * t list

  let index_of { index; _ } = index

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
      ; "duration", option int duration
      ; "project", option string project
      ; "sectors", list string sectors
      ; "label", string label
      ]
  ;;

  let result_to_rensai (curr, full, outdated) =
    let open Rensai.Ast in
    record
      [ "logs", list to_rensai full
      ; "outdated", list to_rensai outdated
      ; "current", to_rensai curr
      ]
  ;;

  let from_rensai =
    let open Rensai.Validation in
    record (fun obj ->
      let open Record in
      let+ start_date = required obj "start_date" Datetime.from_rensai
      and+ index = optional_or ~default:(-1) obj "index" int
      and+ duration = optional obj "duration" int
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

  let push_duration log duration =
    match log.duration with
    | None ->
      let duration = duration * 60 in
      { log with duration = Some duration }
    | Some _ -> log
  ;;

  let compute_duration log dt =
    match log.duration with
    | None when Datetime.Infix.(log.start_date < dt) ->
      let d = Datetime.diff log.start_date dt in
      { log with duration = Some (d |> Int64.to_int) }
    | Some _ | None -> log
  ;;

  let index list =
    list
    |> List.sort (fun { start_date = a; _ } { start_date = b; _ } ->
      Datetime.compare a b)
    |> List.mapi (fun i log -> { log with index = i })
  ;;

  let push sector list = sector :: list |> index

  let split curr list =
    ( curr
    , list
    , List.filter
        (fun log ->
           match log.duration with
           | None -> Datetime.Infix.(curr.start_date > log.start_date)
           | Some _ -> false)
        list )
  ;;

  module Operate = struct
    module Stop = struct
      type t =
        { index : int
        ; duration : int option
        }

      let index { index; _ } = index
      let duration { duration; _ } = duration

      let from_rensai =
        let open Rensai.Validation in
        record (fun obj ->
          let open Record in
          let+ index = required obj "index" (int & Int.is_positive)
          and+ duration = optional obj "duration" (int & Int.is_positive) in
          { index; duration })
      ;;
    end
  end
end
