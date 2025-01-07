type t =
  | Null
  | Unit
  | Bool
  | Char
  | Int
  | Int32
  | Int64
  | Float
  | String
  | Pair of t * t
  | List of t
  | Constr of string * t
  | Record
  | Any
  | Or of t * t
  | And of t * t

let rec equal a b =
  match a, b with
  | Null, Null
  | Unit, Unit
  | Bool, Bool
  | Char, Char
  | Int, Int
  | Int32, Int32
  | Int64, Int64
  | Float, Float
  | String, String
  | Any, Any
  | Record, Record -> true
  | Pair (a, b), Pair (x, y) -> equal a x && equal b y
  | List a, List b -> equal a b
  | Constr (ka, va), Constr (kb, vb) -> String.equal ka kb && equal va vb
  | Or (aa, ba), Or (ab, bb) | And (aa, ba), And (ab, bb) ->
    equal aa ab && equal ba bb
  | Null, _
  | Unit, _
  | Bool, _
  | Char, _
  | Int, _
  | Int32, _
  | Int64, _
  | Float, _
  | String, _
  | Pair (_, _), _
  | List _, _
  | Constr (_, _), _
  | Record, _
  | Any, _
  | Or (_, _), _
  | And (_, _), _ -> false
;;

let from_list_with f = function
  | [] -> Any
  | x :: xs -> List.fold_left (fun comp kind -> f kind comp) x xs
;;

let from_list = from_list_with (fun k c -> Or (k, c))

let rec classify = function
  | Ast.Null -> Null
  | Ast.Unit -> Unit
  | Ast.Bool _ -> Bool
  | Ast.Char _ -> Char
  | Ast.Int _ -> Int
  | Ast.Int32 _ -> Int32
  | Ast.Int64 _ -> Int64
  | Ast.Float _ -> Float
  | Ast.String _ -> String
  | Ast.Pair (a, b) -> Pair (classify a, classify b)
  | Ast.List xs -> List (reduce_row xs)
  | Ast.Constr (k, a) -> Constr (k, classify a)
  | Ast.Record _ -> Record

and reduce_row expr =
  (* hehe the function is very complex in the O sense but...
     I go for an huge reducing instead of implementing [compare]
     for [Kind.t]... *)
  let rec aux acc = function
    | [] -> acc
    | x :: xs ->
      let k = classify x in
      if List.exists (equal k) acc then aux acc xs else aux (k :: acc) xs
  in
  expr |> aux [] |> from_list_with (fun k c -> And (k, c))
;;

module Infix = struct
  let ( || ) a b = Or (a, b)
  let ( && ) a b = And (a, b)
end

include Infix

let rec pp st = function
  | Null -> Fmt.pf st "null"
  | Unit -> Fmt.pf st "unit"
  | Bool -> Fmt.pf st "bool"
  | Char -> Fmt.pf st "char"
  | Int -> Fmt.pf st "int"
  | Int32 -> Fmt.pf st "int32"
  | Int64 -> Fmt.pf st "int64"
  | Float -> Fmt.pf st "float"
  | String -> Fmt.pf st "string"
  | Pair (a, b) -> Fmt.Dump.pair pp pp st (a, b)
  | List xs -> Fmt.pf st "list<%a>" pp xs
  | Constr (k, value) -> Fmt.pf st "%s(%a)" k pp value
  | Record -> Fmt.pf st "?record"
  | Any -> Fmt.pf st "?any"
  | Or (a, b) -> Fmt.pair ~sep:(Fmt.any " |@, ") pp pp st (a, b)
  | And (a, b) -> Fmt.pair ~sep:(Fmt.any " &@, ") pp pp st (a, b)
;;
