let from_string str =
  try str |> Lexing.from_string |> Parser.main Lexer.read with
  | _ -> None
;;
