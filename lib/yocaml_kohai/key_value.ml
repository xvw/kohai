type 'a t = 'a Kohai_model.Key_value.t

let validate subject =
  let open Yocaml.Data.Validation in
  list_of
    (pair string subject
     / record (fun f ->
       let+ key = required f "key" string
       and+ value = required f "value" subject in
       key, value))
  $ Kohai_model.Key_value.from_list
;;

let normalize subject obj =
  let open Yocaml.Data in
  obj
  |> Kohai_model.Key_value.to_list
  |> list_of (fun (k, v) -> record [ "key", string k; "value", subject v ])
;;
