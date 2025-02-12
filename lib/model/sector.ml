type t =
  { name : string
  ; description : string option
  }

let make ?description name = { name; description }

let from_rensai =
  let open Rensai.Validation in
  record (fun obj ->
    let open Record in
    let+ name = required obj "name" String.(string & is_not_blank & is_slug)
    and+ description = optional obj "description" string in
    { name; description })
;;

let to_rensai { name; description } =
  let open Rensai.Ast in
  record [ "name", string name; "description", option string description ]
;;

module Set = struct
  module S = Stdlib.Set.Make (struct
      type nonrec t = t

      let compare { name = a; _ } { name = b; _ } = String.compare a b
    end)

  type t = S.t

  let from_list list =
    list
    |> List.filter_map (fun ast -> ast |> from_rensai |> Result.to_option)
    |> S.of_list
  ;;

  let to_list = S.to_list

  let dump sectors =
    sectors
    |> to_list
    |> List.map (fun sector ->
      Format.asprintf "%a" Rensai.Lang.pp (to_rensai sector))
    |> String.concat "\n"
  ;;

  let to_rensai set = set |> S.to_list |> Rensai.Ast.list to_rensai

  let from_rensai =
    let open Rensai.Validation in
    list_of from_rensai $ S.of_list
  ;;

  let push ({ description; _ } as sector) set =
    match S.find_opt sector set, description with
    | Some { description = None; _ }, _ | Some _, Some _ ->
      set |> S.remove sector |> S.add sector
    | None, _ -> set |> S.add sector
    | Some _, None -> set
  ;;
end
