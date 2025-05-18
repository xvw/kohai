type t = Kohai_model.State.t

let entity_name = "Kohai.State"
let neutral = Yocaml.Metadata.required entity_name

let validate =
  let open Yocaml.Data.Validation in
  record (fun fields ->
    let+ big_bang = optional fields "big_bang" Datetime.validate
    and+ end_of_world = optional fields "end_of_world" Datetime.validate
    and+ number_of_logs = optional_or ~default:0 fields "number_of_logs" int
    and+ duration =
      optional_or
        ~default:Kohai_core.Duration.zero
        fields
        "duration"
        Duration.validate
    in
    Kohai_model.State.make ?big_bang ?end_of_world ~number_of_logs ~duration ())
  / (null $ Kohai_model.State.big_bang)
;;

let normalize state =
  let open Kohai_model.State in
  let open Yocaml.Data in
  record
    [ "big_bang", option Datetime.normalize (big_bang_of state)
    ; "end_of_world", option Datetime.normalize (end_of_world_of state)
    ; "number_of_logs", int (number_of_logs_of state)
    ; "duration", Duration.normalize (duration_of state)
    ]
;;
