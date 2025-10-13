type t = Kohai_model.Url.t

let uri_to_data uri =
  let open Yocaml.Data in
  record
    [ "scheme", option string (uri |> Uri.scheme)
    ; "host", option string (uri |> Uri.host)
    ; "port", option int (uri |> Uri.port)
    ; "path", string (uri |> Uri.path)
    ; "query", list_of (pair string (list_of string)) (uri |> Uri.query)
    ]
;;

let validate =
  let open Yocaml.Data.Validation in
  string $ Uri.of_string
  & fun uri ->
  uri
  |> uri_to_data
  |> record (fun f ->
    let+ scheme =
      required
        f
        "scheme"
        (string
         $ Stdlib.String.lowercase_ascii
         $ Kohai_model.Url.validate_scheme)
    and+ host = required f "host" string
    and+ query =
      optional_or
        ~default:(Kohai_model.Key_value.empty ())
        f
        "query"
        (Key_value.validate (list_of string))
    and+ port = optional f "port" int
    and+ path = required f "path" Path.validate in
    Kohai_model.Url.make ~uri ~scheme ~port ~host ~query ~path ())
;;

let normalize url = url |> Kohai_model.Url.to_rensai |> Yocaml_rensai.normalize
