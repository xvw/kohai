val request_input : ?id:int -> ?params:string -> string -> string
val request_dump : (Rensai.Ast.t, Kohai_core.Error.t) result -> string

val print_result
  :  ?should_fail:bool
  -> (Rensai.Ast.t, Kohai_core.Error.t) result
  -> unit

val step
  :  (module Kohai_core.Sigs.EFFECT_HANDLER)
  -> ?should_fail:bool
  -> id:int ref
  -> ((module Kohai_core.Sigs.EFFECT_HANDLER)
      -> id:int ref
      -> unit
      -> (Rensai.Ast.t, Kohai_core.Error.t) result)
  -> unit

val call
  :  (module Kohai_core.Sigs.EFFECT_HANDLER)
  -> id:int ref
  -> ?params:Rensai.Ast.t
  -> string
  -> (Rensai.Ast.t, Kohai_core.Error.t) result

val call_supervise
  :  (module Kohai_core.Sigs.EFFECT_HANDLER)
  -> id:int ref
  -> path:string
  -> unit
  -> (Rensai.Ast.t, Kohai_core.Error.t) result

val call_supervise_get
  :  (module Kohai_core.Sigs.EFFECT_HANDLER)
  -> id:int ref
  -> unit
  -> (Rensai.Ast.t, Kohai_core.Error.t) result

val call_state_get
  :  (module Kohai_core.Sigs.EFFECT_HANDLER)
  -> id:int ref
  -> unit
  -> (Rensai.Ast.t, Kohai_core.Error.t) result

val call_state_get_for_sector
  :  (module Kohai_core.Sigs.EFFECT_HANDLER)
  -> id:int ref
  -> sector:string
  -> unit
  -> (Rensai.Ast.t, Kohai_core.Error.t) result

val call_state_get_for_project
  :  (module Kohai_core.Sigs.EFFECT_HANDLER)
  -> id:int ref
  -> project:string
  -> unit
  -> (Rensai.Ast.t, Kohai_core.Error.t) result

val call_sector_list
  :  (module Kohai_core.Sigs.EFFECT_HANDLER)
  -> id:int ref
  -> unit
  -> (Rensai.Ast.t, Kohai_core.Error.t) result

val call_sector_save
  :  (module Kohai_core.Sigs.EFFECT_HANDLER)
  -> id:int ref
  -> name:string
  -> ?desc:string
  -> unit
  -> (Rensai.Ast.t, Kohai_core.Error.t) result

val call_sector_delete
  :  (module Kohai_core.Sigs.EFFECT_HANDLER)
  -> id:int ref
  -> name:string
  -> unit
  -> (Rensai.Ast.t, Kohai_core.Error.t) result

val call_project_save
  :  (module Kohai_core.Sigs.EFFECT_HANDLER)
  -> id:int ref
  -> name:string
  -> ?desc:string
  -> unit
  -> (Rensai.Ast.t, Kohai_core.Error.t) result

val call_project_list
  :  (module Kohai_core.Sigs.EFFECT_HANDLER)
  -> id:int ref
  -> unit
  -> (Rensai.Ast.t, Kohai_core.Error.t) result

val call_project_delete
  :  (module Kohai_core.Sigs.EFFECT_HANDLER)
  -> id:int ref
  -> name:string
  -> unit
  -> (Rensai.Ast.t, Kohai_core.Error.t) result

val call_transient_log_list
  :  (module Kohai_core.Sigs.EFFECT_HANDLER)
  -> id:int ref
  -> unit
  -> (Rensai.Ast.t, Kohai_core.Error.t) result

val call_transient_log_record
  :  (module Kohai_core.Sigs.EFFECT_HANDLER)
  -> id:int ref
  -> ?date_query:Kohai_core.Datetime.Query.t
  -> ?project:string
  -> sector:string
  -> label:string
  -> unit
  -> (Rensai.Ast.t, Kohai_core.Error.t) result

val call_transient_log_rewrite
  :  (module Kohai_core.Sigs.EFFECT_HANDLER)
  -> id:int ref
  -> index:int
  -> ?date_query:Kohai_core.Datetime.Query.t
  -> ?project:string
  -> sector:string
  -> label:string
  -> unit
  -> (Rensai.Ast.t, Kohai_core.Error.t) result

val call_transient_log_stop_recording
  :  (module Kohai_core.Sigs.EFFECT_HANDLER)
  -> id:int ref
  -> index:int
  -> ?duration:int
  -> unit
  -> (Rensai.Ast.t, Kohai_core.Error.t) result

val call_transient_log_delete
  :  (module Kohai_core.Sigs.EFFECT_HANDLER)
  -> id:int ref
  -> index:int
  -> unit
  -> (Rensai.Ast.t, Kohai_core.Error.t) result

val call_transient_log_promote
  :  (module Kohai_core.Sigs.EFFECT_HANDLER)
  -> id:int ref
  -> index:int
  -> unit
  -> (Rensai.Ast.t, Kohai_core.Error.t) result

val call_transient_log_add_meta
  :  (module Kohai_core.Sigs.EFFECT_HANDLER)
  -> id:int ref
  -> index:int
  -> key:string
  -> value:string
  -> unit
  -> (Rensai.Ast.t, Kohai_core.Error.t) result

val call_transient_log_add_link
  :  (module Kohai_core.Sigs.EFFECT_HANDLER)
  -> id:int ref
  -> index:int
  -> key:string
  -> value:string
  -> unit
  -> (Rensai.Ast.t, Kohai_core.Error.t) result

val call_transient_log_remove_meta
  :  (module Kohai_core.Sigs.EFFECT_HANDLER)
  -> id:int ref
  -> index:int
  -> key:string
  -> unit
  -> (Rensai.Ast.t, Kohai_core.Error.t) result

val call_transient_log_remove_link
  :  (module Kohai_core.Sigs.EFFECT_HANDLER)
  -> id:int ref
  -> index:int
  -> key:string
  -> unit
  -> (Rensai.Ast.t, Kohai_core.Error.t) result

val call_log_last
  :  (module Kohai_core.Sigs.EFFECT_HANDLER)
  -> id:int ref
  -> unit
  -> (Rensai.Ast.t, Kohai_core.Error.t) result

val call_log_last_for_sector
  :  (module Kohai_core.Sigs.EFFECT_HANDLER)
  -> id:int ref
  -> sector:string
  -> unit
  -> (Rensai.Ast.t, Kohai_core.Error.t) result

val call_log_last_for_project
  :  (module Kohai_core.Sigs.EFFECT_HANDLER)
  -> id:int ref
  -> project:string
  -> unit
  -> (Rensai.Ast.t, Kohai_core.Error.t) result

val call_log_unpromote
  :  (module Kohai_core.Sigs.EFFECT_HANDLER)
  -> id:int ref
  -> uuid:string
  -> unit
  -> (Rensai.Ast.t, Kohai_core.Error.t) result
