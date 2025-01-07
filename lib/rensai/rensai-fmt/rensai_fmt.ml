type ast = Format.formatter -> Rensai.Ast.t -> unit
type kind = Format.formatter -> Rensai.Kind.t -> unit
type value_error = Format.formatter -> Rensai.Validation.value_error -> unit

module Ast = Rensai.Ast
module Kind = Rensai.Kind

let keyword st value = Format.fprintf st "@[<1><%s>@]" value
let parens pf st x = Format.fprintf st "@[<1>(%a)@]" pf x
let box pf st x = Format.fprintf st "@[<1>%a@]" pf x
let brackets pf st x = Format.fprintf st "@[<1>[%a]@]" pf x
let braces pf st x = Format.fprintf st "@[<1>{%a}@]" pf x
let break_with value st () = Format.fprintf st "%s @," value
let pre_break_with value st () = Format.fprintf st "@, %s" value

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

let pp_pair ?(sep = ",") pf st pair =
  let pp st (a, b) = Format.fprintf st "%a%a%a" pf a (break_with sep) () pf b in
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

let rec pp_ast st = function
  | Ast.Null -> pp_null st ()
  | Ast.Unit -> pp_unit st ()
  | Ast.Bool b -> pp_bool st b
  | Ast.Char c -> pp_char st c
  | Ast.Int i -> pp_int st i
  | Ast.Int32 i -> pp_int32 st i
  | Ast.Int64 i -> pp_int64 st i
  | Ast.Float f -> pp_float st f
  | Ast.String s -> pp_string st s
  | Ast.Pair (a, b) -> pp_pair pp_ast st (a, b)
  | Ast.List xs -> pp_list pp_ast st xs
  | Ast.Constr (constr, v) -> pp_constr pp_ast st constr v
  | Ast.Record record -> pp_record pp_ast st record
;;

let pp_kany st () = Format.fprintf st "?any"
let pp_knull st () = Format.fprintf st "null"
let pp_kunit st () = Format.fprintf st "unit"
let pp_kbool st () = Format.fprintf st "bool"
let pp_kchar st () = Format.fprintf st "char"
let pp_kint st () = Format.fprintf st "int"
let pp_kint32 st () = Format.fprintf st "int32"
let pp_kint64 st () = Format.fprintf st "int64"
let pp_kfloat st () = Format.fprintf st "float"
let pp_kstring st () = Format.fprintf st "string"
let pp_kpair pp st pair = pp_pair ~sep:" * " pp st pair
let pp_klist pp st kinds = pp_list pp st [ kinds ]
let pp_kconstr pp st k v = pp_constr pp st k v
let pp_krecord st () = Format.fprintf st "?record"

let pp_kenum sep pp st (l, r) =
  Format.pp_print_list ~pp_sep:(pre_break_with sep) pp st [ l; r ]
;;

let pp_kor pp = box (pp_kenum "| " pp)
let pp_kand pp = box (pp_kenum "& " pp)

let rec pp_kind st = function
  | Kind.Any -> pp_kany st ()
  | Kind.Null -> pp_knull st ()
  | Kind.Unit -> pp_kunit st ()
  | Kind.Bool -> pp_kbool st ()
  | Kind.Char -> pp_kchar st ()
  | Kind.Int -> pp_kint st ()
  | Kind.Int32 -> pp_kint32 st ()
  | Kind.Int64 -> pp_kint64 st ()
  | Kind.Float -> pp_kfloat st ()
  | Kind.String -> pp_kstring st ()
  | Kind.Pair (a, b) -> pp_kpair pp_kind st (a, b)
  | Kind.List kinds -> pp_klist pp_kind st kinds
  | Kind.Constr (k, v) -> pp_kconstr pp_kind st k v
  | Kind.Record -> pp_krecord st ()
  | Kind.Or (l, r) -> pp_kor pp_kind st (l, r)
  | Kind.And (l, r) -> pp_kand pp_kind st (l, r)
;;

let kind_s k = k |> Format.asprintf "%a" pp_kind |> Ast.string

let rec pp_value_error st = function
  | Rensai.Validation.Unexpected_kind { expected; given; value } ->
    let r =
      Ast.record
        [ "expected", kind_s expected; "given", kind_s given; "value", value ]
    in
    Format.fprintf st "Kind @[<1>%a@]" pp_ast r
  | Rensai.Validation.Unexpected_pair { error; given; value } ->
    let r =
      Ast.record
        ([ "given", kind_s given; "value", value ]
         @
         match error with
         | Rensai.Validation.Invalid_both (a, b) ->
           Ast.
             [ "first", string @@ Format.asprintf "@[<1>%a@]" pp_value_error a
             ; "second", string @@ Format.asprintf "@[<1>%a@]" pp_value_error b
             ]
         | Rensai.Validation.Invalid_fst err ->
           Ast.
             [ "first", string @@ Format.asprintf "@[<1>%a@]" pp_value_error err
             ]
         | Rensai.Validation.Invalid_snd err ->
           Ast.
             [ ( "second"
               , string @@ Format.asprintf "@[<1>%a@]" pp_value_error err )
             ])
    in
    Format.fprintf st "Pair (@[<1>%a@])" pp_ast r
;;
