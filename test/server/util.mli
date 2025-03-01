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
  -> ?params:Yojson.t
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
