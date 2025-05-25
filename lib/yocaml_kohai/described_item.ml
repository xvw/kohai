type t = Kohai_model.Described_item.t

let validate =
  let open Yocaml.Data.Validation in
  record (fun f ->
    let+ name = required f "name" string
    and+ description = optional f "description" string
    and+ counter = optional_or f ~default:0 "counter" int in
    Kohai_model.Described_item.make ~counter ?description name)
;;

let normalize di =
  let open Yocaml.Data in
  record
    [ "name", string (Kohai_model.Described_item.name di)
    ; "description", option string (Kohai_model.Described_item.description di)
    ; "counter", int (Kohai_model.Described_item.counter di)
    ]
;;

module Set = struct
  type t = Kohai_model.Described_item.Set.t

  let entity_name = "Kohai.Described_item.Set"
  let neutral = Yocaml.Metadata.required entity_name

  let validate =
    let open Yocaml.Data.Validation in
    (list_of validate $ Kohai_model.Described_item.Set.from_list)
    / (null $ Fun.const Kohai_model.Described_item.Set.empty)
  ;;

  let normalize set =
    set
    |> Kohai_model.Described_item.Set.to_list
    |> Yocaml.Data.list_of normalize
  ;;
end
