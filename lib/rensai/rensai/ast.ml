module S_map = Stdlib.Map.Make (String)

type t =
  | Null
  | Unit
  | Bool of bool
  | Char of char
  | Int of int
  | Int32 of int32
  | Int64 of int64
  | Float of float
  | String of string
  | Pair of t * t
  | List of t list
  | Constr of string * t
  | Record of record

and record = t S_map.t

type 'a conv = 'a -> t

let rec equal a b =
  match a, b with
  | Null, Null | Unit, Unit -> true
  | Bool a, Bool b -> Bool.equal a b
  | Char a, Char b -> Char.equal a b
  | Int a, Int b -> Int.equal a b
  | Int32 a, Int32 b -> Int32.equal a b
  | Int64 a, Int64 b -> Int64.equal a b
  | Float a, Float b -> Float.equal a b
  | String a, String b -> String.equal a b
  | Pair (a, b), Pair (x, y) -> equal a x && equal b y
  | List a, List b -> List.equal equal a b
  | Constr (a, va), Constr (b, vb) -> String.equal a b && equal va vb
  | Record a, Record b -> S_map.equal equal a b
  | Null, _
  | Unit, _
  | Bool _, _
  | Char _, _
  | Int _, _
  | Int32 _, _
  | Int64 _, _
  | Float _, _
  | String _, _
  | Pair (_, _), _
  | List _, _
  | Constr (_, _), _
  | Record _, _ -> false
;;

let record_to_assoc = S_map.to_list
let use f conv x = conv (f x)
let replace x v = use (fun _ -> x) v
let null _ = Null
let unit _ = Unit
let bool b = Bool b
let int i = Int i
let int32 i = Int32 i
let int64 i = Int64 i
let float f = Float f
let string s = String s
let char c = Char c
let pair f g (a, b) = Pair (f a, g b)
let pair' f g a b = pair f g (a, b)
let triple f g h (a, b, c) = pair f (pair g h) (a, (b, c))
let triple' f g h a b c = triple f g h (a, b, c)
let quad f g h i (a, b, c, d) = pair f (pair g (pair h i)) (a, (b, (c, d)))
let quad' f g h i a b c d = quad f g h i (a, b, c, d)
let list f l = List (List.map f l)
let hlist l = List l

let constr f x =
  let k, value = f x in
  Constr (k, value)
;;

let option some = Option.fold ~none:(null ()) ~some

let either left right =
  constr (function
    | Either.Left x -> "left", left x
    | Either.Right x -> "right", right x)
;;

let result ok error =
  constr (function
    | Ok x -> "ok", ok x
    | Error x -> "error", error x)
;;

let record fields =
  let fields = S_map.of_list fields in
  Record fields
;;

module Infix = struct
  let ( <$> ) = use
end

include Infix
