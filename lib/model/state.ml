type t =
  { big_bang : Datetime.t option
  ; end_of_world : Datetime.t option
  }

let patch_date_boundaries datetime state =
  let big_bang =
    match state.big_bang with
    | None -> Some datetime
    | Some previous_datetime ->
      let dt = Datetime.min_of previous_datetime datetime in
      Some dt
  in
  let end_of_world =
    match state.end_of_world with
    | None -> Some datetime
    | Some previous_datetime ->
      let dt = Datetime.max_of previous_datetime datetime in
      Some dt
  in
  { big_bang; end_of_world }
;;

let big_bang () = { big_bang = None; end_of_world = None }

let from_rensai =
  let open Rensai.Validation in
  record (fun fields ->
    let open Record in
    let+ big_bang = optional fields "big_bang" Datetime.from_rensai
    and+ end_of_world = optional fields "end_of_world" Datetime.from_rensai in
    { big_bang; end_of_world })
  / (null $ big_bang)
;;

let to_compact_rensai { big_bang; end_of_world } =
  let open Rensai.Ast in
  record
    [ "big_bang", option Datetime.to_compact_rensai big_bang
    ; "end_of_world", option Datetime.to_compact_rensai end_of_world
    ]
;;
