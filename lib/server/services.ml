type t = string * Jsonrpc.service

module A = Rensai.Ast
module V = Rensai.Validation

let discard = V.const ()

module Experimental = struct
  let prefix = String.cat "experimental/"

  let ping =
    Jsonrpc.service
      ~meth:(prefix "ping")
      ~with_params:discard
      ~finalizer:A.string
      (fun ?id:_ (module _) () -> "pong")
  ;;

  let echo =
    Jsonrpc.service
      ~meth:(prefix "echo")
      ~with_params:V.string
      ~finalizer:A.string
      (fun ?id:_ (module _) input -> input)
  ;;

  let plus =
    Jsonrpc.service
      ~meth:(prefix "plus")
      ~with_params:V.(pair number number)
      ~finalizer:A.float
      (fun ?id:_ (module _) (a, b) -> a +. b)
  ;;
end

module Kohai = struct
  let prefix = String.cat "kohai/"

  let ensure_supervision body =
    Jsonrpc.service
      ~meth:(prefix "ensure_supervision")
      ~with_params:discard
      ~finalizer:A.unit
      (Action.ensure_supervision body)
  ;;
end

let methods body =
  [ Experimental.ping
  ; Experimental.echo
  ; Experimental.plus
  ; Kohai.ensure_supervision body
  ]
;;

let all body =
  Jsonrpc.service
    ~meth:"admin/methods"
    ~with_params:V.(option string)
    ~finalizer:A.(list string)
    (fun ?id:_ (module _) prefix ->
       let methods = "admin/methods" :: List.map fst (methods body) in
       match prefix with
       | None -> methods
       | Some prefix -> List.filter (String.starts_with ~prefix) methods)
  :: methods body
;;
