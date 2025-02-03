type t =
  { name : string
  ; description : string option
  }

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

let push list ({ name; description } as sector) =
  let rec aux acc = function
    | [] -> sector :: list
    | x :: xs when String.equal x.name name ->
      (match description with
       | None -> list
       | Some _ -> (sector :: acc) @ xs)
    | x :: xs -> aux (x :: acc) xs
  in
  list
  |> aux []
  |> List.sort (fun { name = name_a; _ } { name = name_b; _ } ->
    String.compare name_a name_b)
;;
