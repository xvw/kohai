type 'a t = ( :: ) of 'a * 'a list

let make x xs = x :: xs
let singleton x = make x []
let cons x (y :: z) = x :: y :: z

let init len f =
  if len < 1
  then raise @@ Invalid_argument "Nel.init"
  else f 0 :: List.init (len - 1) (fun i -> f @@ (i + 1))
;;

let equal eq (x :: xs) (y :: ys) = eq x y && List.equal eq xs ys

let pp ?(pp_sep = fun st () -> Format.fprintf st "; @,") pp st (x :: xs) =
  Format.pp_print_list ~pp_sep pp st (List.cons x xs)
;;
