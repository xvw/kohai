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
  let supervision = String.cat "kohai/supervision/"

  let ensure_supervision body =
    Jsonrpc.service
      ~meth:(supervision "ensure")
      ~with_params:discard
      ~finalizer:A.unit
      (Action.ensure_supervision body)
  ;;

  let is_valid_supervised_directory =
    Jsonrpc.service
      ~meth:(supervision "is_valid")
      ~with_params:Path.from_rensai
      ~finalizer:A.bool
      Action.is_valid_supervised_directory
  ;;

  let get_supervised_directory =
    Jsonrpc.service
      ~meth:(supervision "get")
      ~with_params:discard
      ~finalizer:A.(option Path.to_rensai)
      Action.get_supervised_directory
  ;;

  let set_supervised_directory body =
    Jsonrpc.service
      ~meth:(supervision "set")
      ~with_params:Path.from_rensai
      ~finalizer:A.null
      (Action.set_supervised_directory body)
  ;;
end

let methods body =
  [ Experimental.ping
  ; Experimental.echo
  ; Experimental.plus
  ; Kohai.ensure_supervision body
  ; Kohai.is_valid_supervised_directory
  ; Kohai.get_supervised_directory
  ; Kohai.set_supervised_directory body
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
