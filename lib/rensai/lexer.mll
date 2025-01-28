{
  open Lexing
  open Parser

  exception Syntax_error of string
}

let int = '-'? ['0'-'9'] ['0'-'9']*
let int32 = '-'? ['0'-'9'] ['0'-'9']* 'l'
let int64 = '-'? ['0'-'9'] ['0'-'9']* 'L'
let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*
let digit = ['0'-'9']
let frac = '.' digit*
let exp = ['e' 'E'] ['-' '+']? digit+
let float = digit* frac? exp? 

rule read = parse
  | white { read lexbuf }
  | newline { new_line lexbuf; read lexbuf }
  | int     { INT (int_of_string (lexeme lexbuf)) }
  | int32   { INT32 (Int32.of_string (lexeme lexbuf)) }
  | int64   { INT64 (Int64.of_string (lexeme lexbuf)) }
  | float   { FLOAT (float_of_string (lexeme lexbuf)) }
  | "null"  { NULL }
  | "true"  { TRUE }
  | "false" { FALSE }
  | "#"     { HASH }
  | "["     { OPEN_LIST }
  | "]"     { CLOSE_LIST }
  | "("     { OPEN_PARENS }
  | ")"     { CLOSE_PARENS }
  | "<"     { OPEN_OBJ }
  | ">"     { CLOSE_OBJ }
  | ":"     { COLON }
  | ";"     { SEMICOLON }
  | ","     { COMMA }
  | "'" [^ '\\'] "'" { CHAR (lexeme_char lexbuf 1) }
  | '"'     { read_string (Buffer.create 17) lexbuf }
  | ['a'-'z' 'A'-'Z' '0'-'9' '\'' '_' '.']+ as w { ATOM w }
  | eof     { EOF }

and read_string buf = parse
  | '"' { STRING (Buffer.contents buf) }
  | '\\' '/'  { Buffer.add_char buf '/'; read_string buf lexbuf }
  | '\\' '\\' { Buffer.add_char buf '\\'; read_string buf lexbuf }
  | '\\' 'b'  { Buffer.add_char buf '\b'; read_string buf lexbuf }
  | '\\' 'f'  { Buffer.add_char buf '\012'; read_string buf lexbuf }
  | '\\' 'n'  { Buffer.add_char buf '\n'; read_string buf lexbuf }
  | '\\' 'r'  { Buffer.add_char buf '\r'; read_string buf lexbuf }
  | '\\' 't'  { Buffer.add_char buf '\t'; read_string buf lexbuf }
  | [^ '"' '\\']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_string buf lexbuf
    }
  | _ { raise (Syntax_error
           ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
  | eof { raise (Syntax_error ("String is not terminated")) }