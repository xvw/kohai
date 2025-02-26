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
        (Operation.Global.ensure_supervision ~body)
    ;;

    let is_valid =
      Jsonrpc.service
        ~meth:(prefix "is_valid")
        ~with_params:Path.from_rensai
        ~finalizer:A.bool
        Operation.Supervised_directory.is_valid
    ;;

    let get =
      Jsonrpc.service
        ~meth:(prefix "get")
        ~with_params:discard
        ~finalizer:A.(option Path.to_rensai)
        Operation.Supervised_directory.get
    ;;

    let set body =
      Jsonrpc.service
        ~meth:(prefix "set")
        ~with_params:Path.from_rensai
        ~finalizer:Path.to_rensai
        (Operation.Supervised_directory.set ~body)
    ;;
  end

  module Described_itm
      (I : Operation.Generic.DESCRIBED_ITEM)
      (P : sig
         val prefix : string
       end) =
  struct
    let prefix = String.cat (prefix P.prefix)

    let list body =
      Jsonrpc.service
        ~meth:(prefix "list")
        ~with_params:discard
        ~finalizer:Kohai_model.Described_item.Set.to_rensai
        (I.list ~body)
    ;;

    let save body =
      Jsonrpc.service
        ~meth:(prefix "save")
        ~with_params:Kohai_model.Described_item.from_rensai
        ~finalizer:Kohai_model.Described_item.Set.to_rensai
        (I.save ~body)
    ;;

    let get body =
      Jsonrpc.service
        ~meth:(prefix "get")
        ~with_params:V.string
        ~finalizer:(A.option Kohai_model.Described_item.to_rensai)
        (I.get ~body)
    ;;

    let delete body =
      Jsonrpc.service
        ~meth:(prefix "delete")
        ~with_params:V.string
        ~finalizer:Kohai_model.Described_item.Set.to_rensai
        (I.delete ~body)
    ;;
  end

  module Sector =
    Described_itm
      (Operation.Sector)
      (struct
        let prefix = "sector/"
      end)

  module Project =
    Described_itm
      (Operation.Project)
      (struct
        let prefix = "project/"
      end)

  module Transient_log = struct
    let prefix = String.cat (prefix "transient-log/")

    let list body =
      Jsonrpc.service
        ~meth:(prefix "list")
        ~with_params:discard
        ~finalizer:Kohai_model.Transient_log.list_to_rensai
        (Operation.Transient_log.list ~body)
    ;;

    let get body =
      Jsonrpc.service
        ~meth:(prefix "get")
        ~with_params:V.int
        ~finalizer:(A.option Kohai_model.Transient_log.to_rensai)
        (Operation.Transient_log.get ~body)
    ;;

    let action body =
      Jsonrpc.service
        ~meth:(prefix "action")
        ~with_params:Kohai_model.Transient_log.operation_from_rensai
        ~finalizer:Kohai_model.Transient_log.result_to_rensai
        (Operation.Transient_log.action ~body)
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
  ; Kohai.Sector.get body
  ; Kohai.Sector.delete body
  ; Kohai.Project.list body
  ; Kohai.Project.save body
  ; Kohai.Project.get body
  ; Kohai.Project.delete body
  ; Kohai.Transient_log.list body
  ; Kohai.Transient_log.get body
  ; Kohai.Transient_log.action body
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
