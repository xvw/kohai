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
  | Record of (string * t) list

type 'a conv = 'a -> t

let use f conv x = conv (f x)
let replace x v = use (fun _ -> x) v
let null = Null
let unit = Unit
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

let constr f x =
  let k, value = f x in
  Constr (k, value)
;;

let option some = Option.fold ~none:null ~some

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

let record fields = Record fields

module Infix = struct
  let ( <$> ) = use
end

include Infix
