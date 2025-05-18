type t = Kohai_core.Path.t

let validate =
  let open Yocaml.Data.Validation in
  string $ Kohai_core.Path.from_string
;;

let normalize path = path |> Kohai_core.Path.to_string |> Yocaml.Data.string
