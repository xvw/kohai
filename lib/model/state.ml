type t =
  { big_bang : Datetime.t option
  ; end_of_world : Datetime.t option
  ; number_of_logs : int
  ; duration : int
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
  { state with big_bang; end_of_world }
;;

let increase_duration amount state =
  { state with duration = state.duration + amount }
;;

let decrease_duration amount state =
  { state with duration = max (state.duration - amount) 0 }
;;

let increase_counter amount state =
  { state with number_of_logs = state.number_of_logs + amount }
;;

let decrease_counter amount state =
  { state with number_of_logs = max (state.number_of_logs - amount) 0 }
;;

let big_bang () =
  { big_bang = None; end_of_world = None; duration = 0; number_of_logs = 0 }
;;

let from_rensai =
  let open Rensai.Validation in
  record (fun fields ->
    let open Record in
    let+ big_bang = optional fields "big_bang" Datetime.from_rensai
    and+ end_of_world = optional fields "end_of_world" Datetime.from_rensai
    and+ number_of_logs = optional_or ~default:0 fields "number_of_logs" int
    and+ duration = optional_or ~default:0 fields "duration" int in
    { big_bang; end_of_world; duration; number_of_logs })
  / (null $ big_bang)
;;

let to_compact_rensai { big_bang; end_of_world; duration; number_of_logs } =
  let open Rensai.Ast in
  record
    [ "big_bang", option Datetime.to_compact_rensai big_bang
    ; "end_of_world", option Datetime.to_compact_rensai end_of_world
    ; "duration", int duration
    ; "number_of_logs", int number_of_logs
    ]
;;

let dump state =
  state |> to_compact_rensai |> Format.asprintf "%a" Rensai.Lang.pp
;;

let from_string string =
  let lexbuf = Lexing.from_string string in
  match
    Option.bind (lexbuf |> Rensai.Lang.from_lexingbuf) (fun x ->
      x |> from_rensai |> Result.to_option)
  with
  | Some x -> x
  | None -> big_bang ()
;;
