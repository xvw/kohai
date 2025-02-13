let from_string str =
  try str |> Lexing.from_string |> Parser.main Lexer.read with
  | _ -> None
;;

let from_lexingbuf lexing_buf =
  try lexing_buf |> Parser.main Lexer.read with
  | _ -> None
;;

let from_lexingbuf_to_list ?(reverse = false) lexing_buf =
  let rec aux acc =
    match from_lexingbuf lexing_buf with
    | None -> acc
    | Some x -> aux (x :: acc)
  in
  let result = aux [] in
  if reverse then List.rev result else result
;;

let surround s1 s2 pp_v ppf v =
  Format.(
    pp_print_string ppf s1;
    pp_v ppf v;
    pp_print_string ppf s2)
;;

let obj pp_v = Fmt.box ~indent:1 (surround "<" ">" pp_v)

let rec pp st = function
  | Ast.Null -> Fmt.pf st "null"
  | Ast.Unit -> Fmt.pf st "()"
  | Ast.Bool x -> Fmt.bool st x
  | Ast.Int x -> Fmt.int st x
  | Ast.Int32 x -> Fmt.int32 st x
  | Ast.Int64 x -> Fmt.int64 st x
  | Ast.Float x -> Fmt.float st x
  | Ast.Char x -> Fmt.pf st "'%a'" Fmt.char x
  | Ast.String x -> Fmt.pf st "\"%a\"" Fmt.string x
  | Ast.Pair (a, b) -> Fmt.Dump.pair pp pp st (a, b)
  | Ast.List xs -> Fmt.brackets (Fmt.list ~sep:Fmt.comma (Fmt.box pp)) st xs
  | Ast.Constr (k, v) -> Fmt.pf st "#%s%a" k (Fmt.parens pp) v
  | Ast.Record record ->
    let fields =
      Fmt.list ~sep:(Fmt.any ";@, ") (fun st (k, v) ->
        Fmt.pf st "@[<1>%s:@ %a@]" k pp v)
    in
    let fields = Fmt.using Ast.record_to_assoc fields in
    (Fmt.box ~indent:2 (obj fields)) st record
;;

let dump_list to_rensai list =
  list
  |> List.map (fun elt -> Format.asprintf "%a" pp (to_rensai elt))
  |> String.concat "\n"
;;
