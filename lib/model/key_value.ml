module M = Stdlib.Map.Make (String)

type 'a t = 'a M.t

let from_rensai subject =
  let open Rensai.Validation in
  list_of
    (pair string subject
     / record (fun b ->
       let open Record in
       let+ key = required b "key" (string & String.is_not_blank)
       and+ value = required b "value" subject in
       key, value))
  $ M.of_list
;;

let to_rensai subject =
  let open Rensai.Ast in
  use
    M.to_list
    (list (fun (k, v) -> record [ "key", string k; "value", subject v ]))
;;

let empty () = M.empty
let add key value map = M.add key value map
let remove key map = M.remove key map
let keys map = map |> M.bindings |> List.map fst
let from_list = M.of_list
let to_list = M.to_list
