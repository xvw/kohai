type scheme =
  | Http
  | Https
  | Ftp
  | Gemini
  | Other of string

type t =
  { scheme : scheme
  ; host : string
  ; path : Path.t
  ; port : int option
  ; query : string list Key_value.t
  ; uri : Uri.t
  }

let uri_to_rensai uri =
  let open Rensai.Ast in
  record
    [ "scheme", option string (uri |> Uri.scheme)
    ; "port", option int (uri |> Uri.port)
    ; "path", string (uri |> Uri.path)
    ; "query", list (pair string (list string)) (uri |> Uri.query)
    ]
;;

let validate_scheme = function
  | "http" -> Http
  | "https" -> Https
  | "ftp" -> Ftp
  | "gemini" -> Gemini
  | x -> Other x
;;

let from_string s =
  let uri = Uri.of_string s in
  let open Rensai.Validation in
  uri
  |> uri_to_rensai
  |> record (fun fields ->
    let open Record in
    let+ scheme =
      required
        fields
        "scheme"
        (string & String.is_not_blank & String.downcase $ validate_scheme)
    and+ host = required fields "host" (string & String.is_not_blank)
    and+ query =
      optional_or
        ~default:(Key_value.empty ())
        fields
        "query"
        (Key_value.from_rensai (list_of string))
    and+ port = optional fields "port" (int & Int.is_positive & Int.is_not_null)
    and+ path = required fields "path" Path.from_rensai in
    { uri; scheme; port; host; query; path })
;;

let from_rensai = Rensai.Validation.(string & from_string)

let scheme_to_string = function
  | Http -> "http"
  | Https -> "https"
  | Ftp -> "ftp"
  | Gemini -> "gemini"
  | Other x -> x
;;

let url_repr { host; path; _ } =
  Format.asprintf "%s%s" host (Path.to_string path)
;;

let to_rensai ({ uri; scheme; port; host; query; path } as u) =
  let open Rensai.Ast in
  record
    [ "scheme", string @@ scheme_to_string scheme
    ; "port", option int port
    ; "host", string host
    ; "path", Path.to_rensai path
    ; "query", Key_value.to_rensai (list string) query
    ; "uri", uri_to_rensai uri
    ; "url", string @@ Uri.to_string uri
    ; "repr", string @@ url_repr u
    ]
;;

let to_uri { uri; _ } = uri
let to_compact_rensai { uri; _ } = Rensai.Ast.string (Uri.to_string uri)
