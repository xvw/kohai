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
      (fun (module _) () -> "pong")
  ;;

  let echo =
    Jsonrpc.service
      ~meth:(prefix "echo")
      ~with_params:V.string
      ~finalizer:A.string
      (fun (module _) input -> input)
  ;;

  let plus =
    Jsonrpc.service
      ~meth:(prefix "plus")
      ~with_params:V.(pair number number)
      ~finalizer:A.float
      (fun (module _) (a, b) -> a +. b)
  ;;
end

module Kohai = struct
  let prefix = String.cat "kohai/"

  module Supervision = struct
    let prefix = String.cat (prefix "supervision/")

    let ensure =
      Jsonrpc.service
        ~meth:(prefix "ensure")
        ~with_params:discard
        ~finalizer:A.unit
        Workflow.Supervised_directory.ensure
    ;;

    let is_valid =
      Jsonrpc.service
        ~meth:(prefix "is_valid")
        ~with_params:Path.from_rensai
        ~finalizer:A.bool
        Workflow.Supervised_directory.is_valid
    ;;

    let get =
      Jsonrpc.service
        ~meth:(prefix "get")
        ~with_params:discard
        ~finalizer:A.(option Path.to_rensai)
        Workflow.Supervised_directory.get
    ;;

    let set =
      Jsonrpc.service
        ~meth:(prefix "set")
        ~with_params:Path.from_rensai
        ~finalizer:Path.to_rensai
        Workflow.Supervised_directory.set
    ;;
  end

  module Described_item
      (I : Workflow.Described_item.S)
      (P : sig
         val prefix : string
       end) =
  struct
    let prefix = String.cat (prefix P.prefix)

    let list =
      Jsonrpc.service
        ~meth:(prefix "list")
        ~with_params:discard
        ~finalizer:Kohai_model.Described_item.Set.to_rensai
        I.list
    ;;

    let save =
      Jsonrpc.service
        ~meth:(prefix "save")
        ~with_params:Kohai_model.Described_item.from_rensai
        ~finalizer:Kohai_model.Described_item.Set.to_rensai
        I.save
    ;;

    let get =
      Jsonrpc.service
        ~meth:(prefix "get")
        ~with_params:V.string
        ~finalizer:(A.option Kohai_model.Described_item.to_rensai)
        I.get
    ;;

    let delete =
      Jsonrpc.service
        ~meth:(prefix "delete")
        ~with_params:V.string
        ~finalizer:Kohai_model.Described_item.Set.to_rensai
        I.delete
    ;;
  end

  module Sector =
    Described_item
      (Operation.Sector)
      (struct
        let prefix = "sector/"
      end)

  module Project =
    Described_item
      (Operation.Project)
      (struct
        let prefix = "project/"
      end)

  module Transient_log = struct
    let prefix = String.cat (prefix "transient-log/")

    let list =
      Jsonrpc.service
        ~meth:(prefix "list")
        ~with_params:discard
        ~finalizer:Kohai_model.Transient_log.list_to_rensai
        Operation.Transient_log.list
    ;;

    let get =
      Jsonrpc.service
        ~meth:(prefix "get")
        ~with_params:V.int
        ~finalizer:(A.option Kohai_model.Transient_log.to_rensai)
        Operation.Transient_log.get
    ;;

    let action =
      Jsonrpc.service
        ~meth:(prefix "action")
        ~with_params:Kohai_model.Transient_log.operation_from_rensai
        ~finalizer:Kohai_model.Transient_log.result_to_rensai
        Operation.Transient_log.action
    ;;
  end

  module State = struct
    let prefix = String.cat (prefix "state/")

    let get =
      Jsonrpc.service
        ~meth:(prefix "get")
        ~with_params:discard
        ~finalizer:Kohai_model.State.to_compact_rensai
        Operation.State.get
    ;;

    let get_for_sector =
      Jsonrpc.service
        ~meth:(prefix "get/sector")
        ~with_params:V.string
        ~finalizer:Kohai_model.State.to_compact_rensai
        Operation.State.get_for_sector
    ;;

    let get_for_project =
      Jsonrpc.service
        ~meth:(prefix "get/project")
        ~with_params:V.string
        ~finalizer:Kohai_model.State.to_compact_rensai
        Operation.State.get_for_project
    ;;
  end

  module Log = struct
    let prefix = String.cat (prefix "log/")

    let get =
      Jsonrpc.service
        ~meth:(prefix "get")
        ~with_params:Uuid.from_rensai
        ~finalizer:(A.option Kohai_model.Log.return_rensai)
        Operation.Log.get
    ;;

    let last =
      Jsonrpc.service
        ~meth:(prefix "last")
        ~with_params:discard
        ~finalizer:Kohai_model.Log.list_to_rensai
        Operation.Log.get_last
    ;;

    let last_for_sector =
      Jsonrpc.service
        ~meth:(prefix "last/sector")
        ~with_params:V.string
        ~finalizer:Kohai_model.Log.list_to_rensai
        Operation.Log.get_last_for_sector
    ;;

    let last_for_project =
      Jsonrpc.service
        ~meth:(prefix "last/project")
        ~with_params:V.string
        ~finalizer:Kohai_model.Log.list_to_rensai
        Operation.Log.get_last_for_project
    ;;

    let unpromote =
      Jsonrpc.service
        ~meth:(prefix "unpromote")
        ~with_params:Uuid.from_rensai
        ~finalizer:Kohai_model.Transient_log.list_to_rensai
        Operation.Cross_log.unpromote_log
    ;;
  end
end

let methods =
  [ Experimental.ping
  ; Experimental.echo
  ; Experimental.plus
  ; Kohai.Supervision.ensure
  ; Kohai.Supervision.is_valid
  ; Kohai.Supervision.get
  ; Kohai.Supervision.set
  ; Kohai.Sector.list
  ; Kohai.Sector.save
  ; Kohai.Sector.get
  ; Kohai.Sector.delete
  ; Kohai.Project.list
  ; Kohai.Project.save
  ; Kohai.Project.get
  ; Kohai.Project.delete
  ; Kohai.Transient_log.list
  ; Kohai.Transient_log.get
  ; Kohai.Transient_log.action
  ; Kohai.State.get
  ; Kohai.State.get_for_sector
  ; Kohai.State.get_for_project
  ; Kohai.Log.get
  ; Kohai.Log.last
  ; Kohai.Log.last_for_sector
  ; Kohai.Log.last_for_project
  ; Kohai.Log.unpromote
  ]
;;

let all =
  Jsonrpc.service
    ~meth:"admin/methods"
    ~with_params:V.(option string)
    ~finalizer:A.(list string)
    (fun (module _) prefix ->
       let methods = "admin/methods" :: List.map fst methods in
       match prefix with
       | None -> methods
       | Some prefix -> List.filter (String.starts_with ~prefix) methods)
  :: methods
;;
