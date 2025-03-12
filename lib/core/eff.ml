module type HANDLER = Sigs.EFFECT_HANDLER

type handler = (module HANDLER)

module Handler (R : Sigs.EFFECT_REQUIREMENT) : HANDLER = struct
  include R

  exception Handler_exn of Error.custom

  let raise error = raise (Handler_exn error)

  let handle_with_error program =
    try Ok (program ()) with
    | Handler_exn error -> Error error
    | exn -> Error (Error.unknown_error ~message:(Printexc.to_string exn) ())
  ;;
end

let raise (module H : HANDLER) error = H.raise error

let set_supervised_directory (module H : HANDLER) potential_path =
  H.set_supervised_directory potential_path
;;

let get_supervised_directory (module H : HANDLER) =
  H.get_supervised_directory ()
;;

let from_result (module H : HANDLER) callback = function
  | Ok x -> x
  | Error err -> raise (module H) (callback err)
;;

let exists (module H : HANDLER) path = H.exists path
let is_file (module H : HANDLER) path = H.is_file path
let is_dir (module H : HANDLER) path = H.is_dir path
let read_file (module H : HANDLER) path = H.read_file path

let create_dir (module H : HANDLER) path =
  let rec aux path =
    if is_file (module H) path
    then ()
    else if not (is_dir (module H) path)
    then (
      let () =
        match Path.parent path with
        | None -> ()
        | Some parent -> aux parent
      in
      H.create_dir path)
    else ()
  in
  aux path
;;

let write_file (module H : HANDLER) path content =
  match Path.parent path with
  | Some parent ->
    let () = create_dir (module H) parent in
    H.write_file path content
  | None -> H.write_file path content
;;

let append_to_file (module H : HANDLER) path content =
  match Path.parent path with
  | Some parent ->
    let () = create_dir (module H) parent in
    H.append_to_file path content
  | None -> H.append_to_file path content
;;

let now (module H : HANDLER) =
  let time = H.now () in
  time
  |> H.datetime_from_float
  |> from_result (module H) (Error.invalid_datetime time)
;;

let delete (module H : HANDLER) path =
  if is_dir (module H) path
  then H.delete_dir ~recursive:true path
  else H.delete_file path
;;

let handle (module H : HANDLER) program =
  let program () = program (module H : HANDLER) in
  H.handle_with_error program
;;
