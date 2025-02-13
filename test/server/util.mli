val request_input : ?id:int -> ?params:string -> string -> string

val request_dump
  :  (Rensai.Ast.t, Kohai_core.Sigs.jsonrpc_error) result
  -> string
