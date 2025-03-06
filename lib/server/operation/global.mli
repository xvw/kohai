(** Global operation related to action and Jsonrpc handlers. *)

(** Check that a path is a valid candidate for supervision. *)
val check_supervised_path
  :  ?id:int
  -> body:string
  -> (module Sigs.EFFECT_HANDLER)
  -> Path.t
  -> Path.t

(** Ensure that an action is guarded by a supervised directory. *)
val ensure_supervision
  :  ?id:int
  -> body:string
  -> (module Sigs.EFFECT_HANDLER)
  -> unit
  -> Path.t

(** Perform an operation guarded by the presence of a supervised directory. *)
val with_supervision
  :  ?id:int
  -> body:string
  -> (module Sigs.EFFECT_HANDLER)
  -> (?id:int -> body:string -> (module Sigs.EFFECT_HANDLER) -> 'a -> 'b)
  -> 'a
  -> 'b
