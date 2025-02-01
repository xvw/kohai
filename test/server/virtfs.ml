open Kohai_core

type 'a elt =
  { name : string
  ; mtime : int
  ; content : 'a
  }

type item =
  | File of string elt
  | Directory of t elt

and t = item list

let name_of = function
  | File { name; _ } | Directory { name : string; _ } -> name
;;

let mtime_of = function
  | File { mtime; _ } | Directory { mtime; _ } -> mtime
;;

let has_name name elt = String.equal name (name_of elt)

(* The goal of the comparison function is to be consistent when
   creating file system to preserve equality even if we traverse the
   full file-tree. *)
let compare_item a b =
  let name x = String.lowercase_ascii @@ name_of x in
  match a, b with
  | File _, File _ | Directory _, Directory _ ->
    String.compare (name a) (name b)
  | File _, Directory _ -> 1
  | Directory _, File _ -> -1
;;

let from_list_aux = List.sort compare_item
let file ?(mtime = 0) ?(content = "") name = File { name; mtime; content }

let dir ?mtime name children =
  let mtime =
    match mtime with
    | None ->
      List.fold_left (fun p child -> Int.max p (mtime_of child)) 0 children
    | Some x -> x
  in
  let content = from_list_aux children in
  Directory { name; mtime; content }
;;

let from_list ?mtime children = [ dir ?mtime "" children ]

let path_to_list p =
  if Path.is_relative p then Path.to_list p else "" :: Path.to_list p
;;

let get fs path =
  let rec aux fs path =
    match fs, path with
    | x :: xs, [ p ] -> if has_name p x then Some x else aux xs path
    | (Directory { content; _ } as x) :: xs, p :: ps ->
      if has_name p x then aux content ps else aux xs path
    | _ :: xs, path -> aux xs path
    | [], _ -> None
  in
  aux fs (path_to_list path)
;;

let extract_target path =
  path
  |> Path.as_target
  |> Option.map (fun (xs, target) ->
    (if Path.is_relative path then xs else "" :: xs), target)
;;

let fold_callback acc = function
  | None -> from_list_aux acc
  | Some x -> from_list_aux (x :: acc)
;;

let update fs path callback =
  match extract_target path with
  | None -> fs
  | Some (path, target) ->
    let rec aux acc fs path =
      match fs, path with
      | [], [] -> callback ~target ?previous:None () |> fold_callback acc
      | item :: fs_xs, [] ->
        if has_name target item
        then (
          let new_acc = acc @ fs_xs in
          callback ~target ?previous:(Some item) () |> fold_callback new_acc)
        else aux (item :: acc) fs_xs []
      | (Directory { name; content; _ } as cdir) :: fs_xs, fragment :: path_xs
        ->
        if has_name fragment cdir
        then (
          let new_dir = dir name (aux [] content path_xs) in
          new_dir :: (acc @ fs_xs) |> from_list_aux)
        else aux (cdir :: acc) fs_xs path
      | [], fragment :: path_xs ->
        let new_dir = dir fragment (aux [] [] path_xs) in
        new_dir :: acc |> from_list_aux
      | x :: fs_xs, path -> aux (x :: acc) fs_xs path
    in
    aux [] fs path
;;

let cat fs path =
  match get fs path with
  | None ->
    path
    |> Path.to_string
    |> Format.asprintf "cat: %s: No such file or directory"
  | Some (Directory _) ->
    path |> Path.to_string |> Format.asprintf "cat: %s: Is a directory"
  | Some (File { content; _ }) -> content
;;

module Make (H : sig
    val fs : t
  end) =
struct
  let supervised_directory = ref None
  let fs = ref H.fs

  let exists path =
    match get !fs path with
    | Some _ -> true
    | None -> false
  ;;

  let is_file path =
    match get !fs path with
    | Some (File _) -> true
    | None | Some _ -> false
  ;;

  let is_dir path =
    match get !fs path with
    | Some (Directory _) -> true
    | None | Some _ -> false
  ;;

  let read_file path =
    match get !fs path with
    | None | Some (Directory _) -> ""
    | Some (File { content; _ }) -> content
  ;;

  let create_dir path =
    let new_fs =
      update !fs path (fun ~target ?previous:_ () -> Some (dir target []))
    in
    fs := new_fs
  ;;

  let write_file path content =
    let new_fs =
      update !fs path (fun ~target ?previous:_ () ->
        Some (file ~content target))
    in
    fs := new_fs
  ;;

  let append_to_file path content =
    let new_fs =
      update !fs path (fun ~target ?previous () ->
        match previous with
        | Some (Directory _) -> None
        | Some (File { content = x; _ }) ->
          Some (file ~content:(content ^ x) target)
        | None -> Some (file ~content target))
    in
    fs := new_fs
  ;;

  let prepend_to_file path content =
    let new_fs =
      update !fs path (fun ~target ?previous () ->
        match previous with
        | Some (Directory _) -> None
        | Some (File { content = x; _ }) ->
          Some (file ~content:(content ^ x) target)
        | None -> Some (file ~content target))
    in
    fs := new_fs
  ;;

  let set_supervised_directory v = supervised_directory := v
  let get_supervised_directory () = !supervised_directory
end
