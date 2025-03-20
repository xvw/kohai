type t = int64

type representation =
  { d : t
  ; h : t
  ; m : t
  ; s : t
  }

let compare = Int64.compare
let from_int = Int64.of_int
let from_int32 = Int64.of_int32
let from_float = Int64.of_float
let from_int64 x = x
let to_int = Int64.to_int
let to_int32 = Int64.to_int32
let to_int64 x = x
let to_float = Int64.to_float
let to_rensai = Rensai.Ast.int64

let compute x =
  let ( - ) = Int64.sub
  and ( * ) = Int64.mul in
  let d = Int64.div x 86400L in
  let h = Int64.div (x - (d * 86400L)) 3600L in
  let m = Int64.div (x - (d * 86400L) - (h * 3600L)) 60L in
  let s = x - (d * 86400L) - (h * 3600L) - (m * 60L) in
  { d; h; m; s }
;;

let pp st duration =
  let { d; h; m; s } = compute duration in
  let sum others = List.fold_left Int64.add Int64.zero others in
  let pp_with_suffix suff others st x =
    let coma = if Int64.(equal zero (sum others)) then "" else ", " in
    if Int64.(equal zero x)
    then Format.fprintf st ""
    else Format.fprintf st "%Ld%s%s" x suff coma
  in
  let pp_seconds others st x =
    if Int64.(equal zero x && not (equal (sum others) zero))
    then Format.fprintf st ""
    else Format.fprintf st "%Lds" x
  in
  Format.fprintf
    st
    "%a%a%a%a"
    (pp_with_suffix "d" [ h; m; s ])
    d
    (pp_with_suffix "h" [ m; s ])
    h
    (pp_with_suffix "m" [ s ])
    m
    (pp_seconds [ d; h; m ])
    s
;;

let add a b = Int64.add a b
let zero = Int64.zero
let sub a b = Int64.sub a b
let bound_positive x = Int64.max x zero

module Query = struct end
