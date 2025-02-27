type t = Uuidm.t

let gen s = Uuidm.v5 Uuidm.ns_oid s

let from_rensai =
  let open Rensai.Validation in
  string
  & fun str ->
  str
  |> Uuidm.of_string
  |> function
  | None -> fail_with ~subject:str "Not a valid uuid"
  | Some x -> Ok x
;;

let to_rensai id =
  let open Rensai.Ast in
  id |> Uuidm.to_string |> string
;;

let to_string s = Uuidm.to_string s

module Set = struct
  module S = Stdlib.Set.Make (Uuidm)

  type t = S.t

  let from_list list =
    list
    |> List.filter_map (fun ast -> ast |> from_rensai |> Result.to_option)
    |> S.of_list
  ;;

  let to_list = S.to_list
  let dump items = items |> to_list |> Rensai.Lang.dump_list to_rensai
  let to_rensai set = set |> S.to_list |> Rensai.Ast.list to_rensai

  let from_rensai =
    let open Rensai.Validation in
    list_of from_rensai $ S.of_list
  ;;

  let push uid set = S.add uid set
  let remove uid set = S.remove uid set
end
