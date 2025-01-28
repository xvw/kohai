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
