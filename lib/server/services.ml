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

  module Supervision = struct
    let prefix = String.cat (prefix "supervision/")

    let ensure body =
      Jsonrpc.service
        ~meth:(prefix "ensure")
        ~with_params:discard
        ~finalizer:A.unit
        (Action.ensure_supervision body)
    ;;

    let is_valid =
      Jsonrpc.service
        ~meth:(prefix "is_valid")
        ~with_params:Path.from_rensai
        ~finalizer:A.bool
        Action.is_valid_supervised_directory
    ;;

    let get =
      Jsonrpc.service
        ~meth:(prefix "get")
        ~with_params:discard
        ~finalizer:A.(option Path.to_rensai)
        Action.get_supervised_directory
    ;;

    let set body =
      Jsonrpc.service
        ~meth:(prefix "set")
        ~with_params:Path.from_rensai
        ~finalizer:A.null
        (Action.set_supervised_directory body)
    ;;
  end

  module Sector = struct
    let prefix = String.cat (prefix "sector/")

    let list body =
      Jsonrpc.service
        ~meth:(prefix "list")
        ~with_params:discard
        ~finalizer:Kohai_model.Sector.Set.to_rensai
        (Action.get_sectors body)
    ;;

    let save body =
      Jsonrpc.service
        ~meth:(prefix "save")
        ~with_params:Kohai_model.Sector.from_rensai
        ~finalizer:Kohai_model.Sector.Set.to_rensai
        (Action.save_sector body)
    ;;
  end

  module Log = struct
    let prefix = String.cat (prefix "log/")

    let record body =
      Jsonrpc.service
        ~meth:(prefix "record")
        ~with_params:Kohai_model.Log.Recored.from_rensai
        ~finalizer:(A.list Kohai_model.Log.Transient.to_rensai)
        (Action.record_log body)
    ;;

    let transient body =
      Jsonrpc.service
        ~meth:(prefix "transient")
        ~with_params:discard
        ~finalizer:(A.list Kohai_model.Log.Transient.to_rensai)
        (Action.get_transient_log body)
    ;;
  end
end

let methods body =
  [ Experimental.ping
  ; Experimental.echo
  ; Experimental.plus
  ; Kohai.Supervision.ensure body
  ; Kohai.Supervision.is_valid
  ; Kohai.Supervision.get
  ; Kohai.Supervision.set body
  ; Kohai.Sector.list body
  ; Kohai.Sector.save body
  ; Kohai.Log.record body
  ; Kohai.Log.transient body
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
