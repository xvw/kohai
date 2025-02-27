type t =
  { name : string
  ; description : string option
  ; counter : int
  }

let make ?(counter = 0) ?description name = { name; description; counter }
let can_be_erased { counter; _ } = counter <= 0

let from_rensai =
  let open Rensai.Validation in
  record (fun obj ->
    let open Record in
    let+ name = required obj "name" String.(string & is_not_blank & is_slug)
    and+ description = optional obj "description" string
    and+ counter = optional_or ~default:0 obj "counter" int in
    { name; description; counter })
;;

let to_rensai { name; description; counter } =
  let open Rensai.Ast in
  record
    [ "name", string name
    ; "description", option string description
    ; "counter", int counter
    ]
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
  let dump items = items |> to_list |> Rensai.Lang.dump_list to_rensai
  let to_rensai set = set |> S.to_list |> Rensai.Ast.list to_rensai

  let from_rensai =
    let open Rensai.Validation in
    list_of from_rensai $ S.of_list
  ;;

  let push ({ description; _ } as item) set =
    match S.find_opt item set, description with
    | Some { description = None; _ }, _ | Some _, Some _ ->
      set |> S.remove item |> S.add item
    | None, _ -> set |> S.add item
    | Some _, None -> set
  ;;

  let find name set = S.find_opt { name; description = None; counter = 0 } set
  let remove name set = S.remove { name; description = None; counter = 0 } set

  let increase name set =
    match find name set with
    | None -> S.add { name; description = None; counter = 0 } set
    | Some item ->
      set |> remove name |> S.add { item with counter = succ item.counter }
  ;;

  let decrease name set =
    match find name set with
    | None -> S.add { name; description = None; counter = 0 } set
    | Some item ->
      set
      |> remove name
      |> S.add { item with counter = max (pred item.counter) 0 }
  ;;
end
