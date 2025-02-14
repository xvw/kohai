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
let lpair a b = Pair (a, b)
let pair f g (a, b) = Pair (f a, g b)
let pair' f g a b = pair f g (a, b)
let triple f g h (a, b, c) = pair f (pair g h) (a, (b, c))
let triple' f g h a b c = triple f g h (a, b, c)
let quad f g h i (a, b, c, d) = pair f (pair g (pair h i)) (a, (b, (c, d)))
let quad' f g h i a b c d = quad f g h i (a, b, c, d)
let list f l = List (List.map f l)
let hlist l = List l
let lconstr k v = Constr (String.trim @@ String.lowercase_ascii k, v)

let constr f x =
  let k, value = f x in
  Constr (String.trim @@ String.lowercase_ascii k, value)
;;

let sum f x = constr f x
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

let rec pp st = function
  | Null -> Fmt.pf st "<null>"
  | Unit -> Fmt.pf st "<unit>"
  | Bool x -> Fmt.bool st x
  | Char c -> Fmt.pf st "%C" c
  | Int i -> Fmt.int st i
  | Int32 i -> Fmt.int32 st i
  | Int64 i -> Fmt.int64 st i
  | Float f -> Fmt.float st f
  | String s -> Fmt.pf st "%S" s
  | Pair (a, b) -> Fmt.Dump.pair pp pp st (a, b)
  | List xs -> Fmt.Dump.list pp st xs
  | Constr (constr, value) -> Fmt.pf st "%s%a" constr (Fmt.parens pp) value
  | Record record ->
    let fields =
      Fmt.list ~sep:(Fmt.any ";@, ") (fun st (k, v) ->
        Fmt.pf st "@[<1>%s =@ %a@]" k pp v)
    in
    let fields = Fmt.using record_to_assoc fields in
    (Fmt.box ~indent:2 (Fmt.braces fields)) st record
;;
