type t = private
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

val use : ('a -> 'b) -> 'b conv -> 'a conv
val replace : 'b -> 'b conv -> 'a conv
val null : t
val unit : t
val bool : bool conv
val int : int conv
val int32 : int32 conv
val int64 : int64 conv
val float : float conv
val char : char conv
val string : string conv
val pair : 'a conv -> 'b conv -> ('a * 'b) conv
val pair' : 'a conv -> 'b conv -> 'a -> 'b conv
val triple : 'a conv -> 'b conv -> 'c conv -> ('a * 'b * 'c) conv
val triple' : 'a conv -> 'b conv -> 'c conv -> 'a -> 'b -> 'c conv
val quad : 'a conv -> 'b conv -> 'c conv -> 'd conv -> ('a * 'b * 'c * 'd) conv

val quad'
  :  'a conv
  -> 'b conv
  -> 'c conv
  -> 'd conv
  -> 'a
  -> 'b
  -> 'c
  -> 'd conv

val record : (string * t) list conv
val list : 'a conv -> 'a list conv
val constr : ('a -> string * t) -> 'a conv
val option : 'a conv -> 'a option conv
val either : 'left conv -> 'right conv -> ('left, 'right) Either.t conv
val result : 'ok conv -> 'err conv -> ('ok, 'err) result conv
