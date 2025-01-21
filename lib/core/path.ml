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

let from_rensai =
  let open Rensai.Validation in
  (string $ Stdlib.String.split_on_char '/') / list_of string
  $ function
  | "" :: xs -> Absolute (Stdlib.List.rev xs)
  | "." :: xs | xs -> Relative (Stdlib.List.rev xs)
;;

let is_absolute = function
  | Absolute _ -> true
  | _ -> false
;;

let is_relative = function
  | Relative _ -> true
  | _ -> false
;;

let from_string x =
  match String.split_on_char '/' x with
  | "" :: xs -> Absolute (List.rev xs)
  | "." :: xs | xs -> Relative (List.rev xs)
;;
