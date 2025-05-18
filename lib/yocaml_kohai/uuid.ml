type t = Kohai_core.Uuid.t

let validate =
  let open Yocaml.Data.Validation in
  string
  & fun str ->
  str
  |> Kohai_core.Uuid.from_string
  |> Option.fold ~none:(fail_with ~given:str "Not a valid UUID") ~some:Result.ok
;;

let normalize id = id |> Kohai_core.Uuid.to_string |> Yocaml.Data.string

module Set = struct
  type t = Kohai_core.Uuid.Set.t

  let entity_name = "Kohai.Uuid.Set"
  let neutral = Yocaml.Metadata.required entity_name

  let validate =
    let open Yocaml.Data.Validation in
    (list_of validate $ Kohai_core.Uuid.Set.from_list)
    / (null $ Fun.const Kohai_core.Uuid.Set.empty)
  ;;

  let normalize set =
    set |> Kohai_core.Uuid.Set.to_list |> Yocaml.Data.list_of normalize
  ;;
end
