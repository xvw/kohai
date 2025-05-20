type t = Kohai_core.Duration.t

let validate = Yocaml.Data.Validation.(int $ Kohai_core.Duration.from_int)

let normalize duration =
  let open Yocaml.Data in
  let Kohai_core.Duration.{ d; h; m; s } =
    Kohai_core.Duration.compute duration
  in
  record
    [ "duration", int @@ Kohai_core.Duration.to_int duration
    ; "d", int @@ Kohai_core.Duration.to_int d
    ; "h", int @@ Kohai_core.Duration.to_int h
    ; "m", int @@ Kohai_core.Duration.to_int m
    ; "s", int @@ Kohai_core.Duration.to_int s
    ; "repr", string @@ Format.asprintf "%a" Kohai_core.Duration.pp duration
    ]
;;
