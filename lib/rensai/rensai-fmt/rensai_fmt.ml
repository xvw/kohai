type t = Format.formatter -> Rensai.Ast.t -> unit

module Ast = Rensai.Ast

let keyword st value = Format.fprintf st "@[<1><%s>@]" value
let parens pf st x = Format.fprintf st "@[<1>(%a)@]" pf x
let brackets pf st x = Format.fprintf st "@[<1>[%a]@]" pf x
let braces pf st x = Format.fprintf st "@[<1>{%a}@]" pf x
let break_with value st () = Format.fprintf st "%s @," value

let quote ?(enclosing = "\"") pf st x =
  Format.fprintf st "@[<1>@<1>%s%a@<1>%s@]" enclosing pf x enclosing
;;

let pp_specific_int ~suffix f st str = Format.fprintf st "%s%s" (f str) suffix
let pp_null st () = keyword st "null"
let pp_unit st () = keyword st "unit"
let pp_bool st b = Format.pp_print_bool st b
let pp_char st c = quote ~enclosing:"'" Format.pp_print_char st c
let pp_string st s = quote Format.pp_print_string st s
let pp_int st i = Format.pp_print_int st i
let pp_int32 st i = pp_specific_int ~suffix:"l" Int32.to_string st i
let pp_int64 st i = pp_specific_int ~suffix:"L" Int64.to_string st i
let pp_float st f = Format.pp_print_float st f

let pp_pair pf st pair =
  let pp st (a, b) =
    Format.fprintf st "%a %a %a" pf a (break_with ",") () pf b
  in
  parens pp st pair
;;

let pp_list pf st list =
  brackets (Format.pp_print_list ~pp_sep:(break_with ";") pf) st list
;;

let pp_constr pf st constr value =
  let pp st () = Format.fprintf st "%s%a" constr (parens pf) value in
  parens pp st ()
;;

let pp_record pf st record =
  let record = Ast.record_to_assoc record in
  let field st (key, value) =
    Format.fprintf st "@[<1>%s =@ %a@]" key pf value
  in
  braces (Format.pp_print_list ~pp_sep:(break_with ";") field) st record
;;

let rec pp st = function
  | Ast.Null -> pp_null st ()
  | Ast.Unit -> pp_unit st ()
  | Ast.Bool b -> pp_bool st b
  | Ast.Char c -> pp_char st c
  | Ast.Int i -> pp_int st i
  | Ast.Int32 i -> pp_int32 st i
  | Ast.Int64 i -> pp_int64 st i
  | Ast.Float f -> pp_float st f
  | Ast.String s -> pp_string st s
  | Ast.Pair (a, b) -> pp_pair pp st (a, b)
  | Ast.List xs -> pp_list pp st xs
  | Ast.Constr (constr, v) -> pp_constr pp st constr v
  | Ast.Record record -> pp_record pp st record
;;
