type t =
  | Relative of string list
  | Absolute of string list

let pwd = Relative []
let root = Absolute []
let sep = Filename.dir_sep

let equal a b =
  match a, b with
  | Relative a, Relative b | Absolute a, Absolute b ->
    List.equal String.equal a b
  | Relative _, _ | Absolute _, _ -> false
;;

let on f = function
  | Relative l -> Relative (f l)
  | Absolute l -> Absolute (f l)
;;

let from path ~into = on (fun list -> into :: list) path

module Infix = struct
  let ( / ) l r = from l ~into:r
  let ( ~/ ) r = pwd / r
end

include Infix

let concat_fragments fragments =
  fragments
  |> List.fold_left
       (fun (i, acc) fragment ->
          let new_acc =
            if Int.equal i 0 then fragment else fragment ^ sep ^ acc
          in
          succ i, new_acc)
       (0, "")
  |> snd
;;

let to_string = function
  | Relative xs -> "./" ^ concat_fragments xs
  | Absolute xs -> "/" ^ concat_fragments xs
;;

let extension = function
  | Relative [] | Absolute [] -> ""
  | Relative (x :: _) | Absolute (x :: _) -> Filename.extension x
;;

let extension_opt path =
  let ext = extension path in
  if String.equal "" ext then None else Some ext
;;
