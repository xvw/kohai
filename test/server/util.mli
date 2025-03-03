val request_input : ?id:int -> ?params:string -> string -> string

val request_dump
  :  (Rensai.Ast.t, Kohai_core.Sigs.jsonrpc_error) result
  -> string

val print_result
  :  ?should_fail:bool
  -> (Rensai.Ast.t, Kohai_core.Sigs.jsonrpc_error) result
  -> unit

val call
  :  (module Kohai_core.Sigs.EFFECT_HANDLER)
  -> id:int ref
  -> ?params:Rensai.Ast.t
  -> string
  -> (Rensai.Ast.t, Kohai_core.Sigs.jsonrpc_error) result

val call_supervise
  :  (module Kohai_core.Sigs.EFFECT_HANDLER)
  -> id:int ref
  -> path:string
  -> unit
  -> (Rensai.Ast.t, Kohai_core.Sigs.jsonrpc_error) result

val call_supervise_get
  :  (module Kohai_core.Sigs.EFFECT_HANDLER)
  -> id:int ref
  -> unit
  -> (Rensai.Ast.t, Kohai_core.Sigs.jsonrpc_error) result

val call_sector_list
  :  (module Kohai_core.Sigs.EFFECT_HANDLER)
  -> id:int ref
  -> unit
  -> (Rensai.Ast.t, Kohai_core.Sigs.jsonrpc_error) result

val call_sector_save
  :  (module Kohai_core.Sigs.EFFECT_HANDLER)
  -> id:int ref
  -> name:string
  -> ?desc:string
  -> unit
  -> (Rensai.Ast.t, Kohai_core.Sigs.jsonrpc_error) result

val call_sector_delete
  :  (module Kohai_core.Sigs.EFFECT_HANDLER)
  -> id:int ref
  -> name:string
  -> unit
  -> (Rensai.Ast.t, Kohai_core.Sigs.jsonrpc_error) result

val call_project_save
  :  (module Kohai_core.Sigs.EFFECT_HANDLER)
  -> id:int ref
  -> name:string
  -> ?desc:string
  -> unit
  -> (Rensai.Ast.t, Kohai_core.Sigs.jsonrpc_error) result

val call_project_list
  :  (module Kohai_core.Sigs.EFFECT_HANDLER)
  -> id:int ref
  -> unit
  -> (Rensai.Ast.t, Kohai_core.Sigs.jsonrpc_error) result

val call_project_delete
  :  (module Kohai_core.Sigs.EFFECT_HANDLER)
  -> id:int ref
  -> name:string
  -> unit
  -> (Rensai.Ast.t, Kohai_core.Sigs.jsonrpc_error) result

val call_transient_log_list
  :  (module Kohai_core.Sigs.EFFECT_HANDLER)
  -> id:int ref
  -> unit
  -> (Rensai.Ast.t, Kohai_core.Sigs.jsonrpc_error) result

val call_transient_log_record
  :  (module Kohai_core.Sigs.EFFECT_HANDLER)
  -> id:int ref
  -> ?date_query:Kohai_core.Datetime.Query.t
  -> ?project:string
  -> sector:string
  -> label:string
  -> unit
  -> (Rensai.Ast.t, Kohai_core.Sigs.jsonrpc_error) result

val call_transient_log_rewrite
  :  (module Kohai_core.Sigs.EFFECT_HANDLER)
  -> id:int ref
  -> index:int
  -> ?date_query:Kohai_core.Datetime.Query.t
  -> ?project:string
  -> sector:string
  -> label:string
  -> unit
  -> (Rensai.Ast.t, Kohai_core.Sigs.jsonrpc_error) result

val call_transient_log_stop_recording
  :  (module Kohai_core.Sigs.EFFECT_HANDLER)
  -> id:int ref
  -> index:int
  -> ?duration:int
  -> unit
  -> (Rensai.Ast.t, Kohai_core.Sigs.jsonrpc_error) result

val call_transient_log_delete
  :  (module Kohai_core.Sigs.EFFECT_HANDLER)
  -> id:int ref
  -> index:int
  -> unit
  -> (Rensai.Ast.t, Kohai_core.Sigs.jsonrpc_error) result

val call_transient_log_add_meta
  :  (module Kohai_core.Sigs.EFFECT_HANDLER)
  -> id:int ref
  -> index:int
  -> key:string
  -> value:string
  -> unit
  -> (Rensai.Ast.t, Kohai_core.Sigs.jsonrpc_error) result

val call_transient_log_add_link
  :  (module Kohai_core.Sigs.EFFECT_HANDLER)
  -> id:int ref
  -> index:int
  -> key:string
  -> value:string
  -> unit
  -> (Rensai.Ast.t, Kohai_core.Sigs.jsonrpc_error) result

val call_transient_log_remove_meta
  :  (module Kohai_core.Sigs.EFFECT_HANDLER)
  -> id:int ref
  -> index:int
  -> key:string
  -> unit
  -> (Rensai.Ast.t, Kohai_core.Sigs.jsonrpc_error) result

val call_transient_log_remove_link
  :  (module Kohai_core.Sigs.EFFECT_HANDLER)
  -> id:int ref
  -> index:int
  -> key:string
  -> unit
  -> (Rensai.Ast.t, Kohai_core.Sigs.jsonrpc_error) result

val step
  :  (module Kohai_core.Sigs.EFFECT_HANDLER)
  -> ?should_fail:bool
  -> ?desc:string
  -> id:int ref
  -> ((module Kohai_core.Sigs.EFFECT_HANDLER)
      -> id:int ref
      -> unit
      -> (Rensai.Ast.t, Kohai_core.Sigs.jsonrpc_error) result)
  -> unit
