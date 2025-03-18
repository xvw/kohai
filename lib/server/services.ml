type t = string * Jsonrpc.service

module A = struct
  let string _ctx = Rensai.Ast.string
  let float _ctx = Rensai.Ast.float
  let unit _ctx = Rensai.Ast.unit
  let bool _ctx = Rensai.Ast.bool
  let path _ctx = Path.to_rensai
  let opt_path _ctx = Rensai.Ast.(option Path.to_rensai)
  let item_set _ctx = Kohai_model.Described_item.Set.to_rensai
  let opt_item _ctx = Rensai.Ast.option Kohai_model.Described_item.to_rensai
  let opt_transient _ctx = Rensai.Ast.option Kohai_model.Transient_log.to_rensai
  let state _ctx = Kohai_model.State.to_compact_rensai
end

module V = Rensai.Validation

let discard = V.const ()
let no_ctx f (module H : Eff.HANDLER) _ctx = f (module H : Eff.HANDLER)

module Experimental = struct
  let prefix = String.cat "experimental/"

  let ping =
    Jsonrpc.service
      ~meth:(prefix "ping")
      ~with_params:discard
      ~finalizer:A.string
      (fun (module _) _ () -> "pong")
  ;;

  let echo =
    Jsonrpc.service
      ~meth:(prefix "echo")
      ~with_params:V.string
      ~finalizer:A.string
      (fun (module _) _ input -> input)
  ;;

  let plus =
    Jsonrpc.service
      ~meth:(prefix "plus")
      ~with_params:V.(pair number number)
      ~finalizer:A.float
      (fun (module _) _ (a, b) -> a +. b)
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
        (no_ctx Workflow.Supervised_directory.ensure)
    ;;

    let is_valid =
      Jsonrpc.service
        ~meth:(prefix "is_valid")
        ~with_params:Path.from_rensai
        ~finalizer:A.bool
        (no_ctx Workflow.Supervised_directory.is_valid)
    ;;

    let get =
      Jsonrpc.service
        ~meth:(prefix "get")
        ~with_params:discard
        ~finalizer:A.opt_path
        (no_ctx Workflow.Supervised_directory.get)
    ;;

    let set =
      Jsonrpc.service
        ~meth:(prefix "set")
        ~with_params:Path.from_rensai
        ~finalizer:A.path
        (no_ctx Workflow.Supervised_directory.set)
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
        ~finalizer:A.item_set
        (no_ctx I.list)
    ;;

    let save =
      Jsonrpc.service
        ~meth:(prefix "save")
        ~with_params:Kohai_model.Described_item.from_rensai
        ~finalizer:A.item_set
        (no_ctx I.save)
    ;;

    let get =
      Jsonrpc.service
        ~meth:(prefix "get")
        ~with_params:V.string
        ~finalizer:A.opt_item
        (no_ctx I.get)
    ;;

    let delete =
      Jsonrpc.service
        ~meth:(prefix "delete")
        ~with_params:V.string
        ~finalizer:A.item_set
        (no_ctx I.delete)
    ;;
  end

  module Sector =
    Described_item
      (Action.Sector)
      (struct
        let prefix = "sector/"
      end)

  module Project =
    Described_item
      (Action.Project)
      (struct
        let prefix = "project/"
      end)

  module Transient_log = struct
    let prefix = String.cat (prefix "transient-log/")

    let list =
      Jsonrpc.service
        ~meth:(prefix "list")
        ~with_params:discard
        ~finalizer:Kohai_model.Transient_log.Expanded.as_list
        (no_ctx Workflow.Transient_log.list)
    ;;

    let get =
      Jsonrpc.service
        ~meth:(prefix "get")
        ~with_params:V.int
        ~finalizer:A.opt_transient
        (no_ctx Workflow.Transient_log.get)
    ;;

    let action =
      Jsonrpc.service
        ~meth:(prefix "action")
        ~with_params:Kohai_model.Transient_log.operation_from_rensai
        ~finalizer:Kohai_model.Transient_log.Expanded.as_result
        Workflow.Transient_log.action
    ;;
  end

  module State = struct
    let prefix = String.cat (prefix "state/")

    let get =
      Jsonrpc.service
        ~meth:(prefix "get")
        ~with_params:discard
        ~finalizer:A.state
        (no_ctx Workflow.State.get)
    ;;

    let get_for_sector =
      Jsonrpc.service
        ~meth:(prefix "get/sector")
        ~with_params:V.string
        ~finalizer:A.state
        (no_ctx Workflow.State.get_for_sector)
    ;;

    let get_for_project =
      Jsonrpc.service
        ~meth:(prefix "get/project")
        ~with_params:V.string
        ~finalizer:A.state
        (no_ctx Workflow.State.get_for_project)
    ;;
  end

  module Log = struct
    let prefix = String.cat (prefix "log/")

    let get =
      Jsonrpc.service
        ~meth:(prefix "get")
        ~with_params:Uuid.from_rensai
        ~finalizer:Kohai_model.Log.Expanded.as_option
        (no_ctx Workflow.Log.get)
    ;;

    let last =
      Jsonrpc.service
        ~meth:(prefix "last")
        ~with_params:discard
        ~finalizer:Kohai_model.Log.Expanded.as_list
        (no_ctx Workflow.Log.last)
    ;;

    let last_for_sector =
      Jsonrpc.service
        ~meth:(prefix "last/sector")
        ~with_params:V.string
        ~finalizer:Kohai_model.Log.Expanded.as_list
        (no_ctx Workflow.Log.last_for_sector)
    ;;

    let last_for_project =
      Jsonrpc.service
        ~meth:(prefix "last/project")
        ~with_params:V.string
        ~finalizer:Kohai_model.Log.Expanded.as_list
        (no_ctx Workflow.Log.last_for_project)
    ;;

    let unpromote =
      Jsonrpc.service
        ~meth:(prefix "unpromote")
        ~with_params:Uuid.from_rensai
        ~finalizer:Kohai_model.Transient_log.Expanded.as_list
        (no_ctx Workflow.Log.unpromote)
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
    ~finalizer:(fun _ -> Rensai.Ast.(list string))
    (fun (module _) _ctx prefix ->
       let methods = "admin/methods" :: List.map fst methods in
       match prefix with
       | None -> methods
       | Some prefix -> List.filter (String.starts_with ~prefix) methods)
  :: methods
;;
