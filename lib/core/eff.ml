module Handler (R : Sigs.EFFECT_REQUIREMENT) : Sigs.EFFECT_HANDLER = struct
  exception Jsonrpc_exn of Sigs.jsonrpc_error

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

let handle (module H : Sigs.EFFECT_HANDLER) program =
  let program = program (module H : Sigs.EFFECT_HANDLER) in
  H.handle_with_error program
;;
