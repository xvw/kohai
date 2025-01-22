module type HANDLER = Sigs.EFFECT_HANDLER

type handler = (module HANDLER)

module Handler (R : Sigs.EFFECT_REQUIREMENT) : HANDLER = struct
  exception Jsonrpc_exn of Sigs.jsonrpc_error

  include R

  let raise error = raise (Jsonrpc_exn error)

  let handle_with_error program =
    try Ok (program ()) with
    | Jsonrpc_exn error -> Error error
    | exn ->
      Error
        (Custom_error
           { body = ""
           ; id = None
           ; code = 32000
           ; message = Some (Printexc.to_string exn)
           })
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

let handle (module H : HANDLER) program =
  let program () = program (module H : HANDLER) in
  H.handle_with_error program
;;
