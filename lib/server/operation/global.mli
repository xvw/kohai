(** Global operation related to action and Jsonrpc handlers. *)

(** Check that a path is a valid candidate for supervision. *)
val check_supervised_path
  :  ?body:string
  -> ?id:int
  -> (module Sigs.EFFECT_HANDLER)
  -> Path.t
  -> Path.t

(** Ensure that an action is guarded by a supervised directory. *)
val ensure_supervision
  :  ?body:string
  -> ?id:int
  -> (module Sigs.EFFECT_HANDLER)
  -> unit
  -> Path.t

(** Perform an operation guarded by the presence of a supervised directory. *)
val with_supervision
  :  ?body:string
  -> ?id:int
  -> (module Sigs.EFFECT_HANDLER)
  -> (?body:string -> ?id:int -> (module Sigs.EFFECT_HANDLER) -> 'a -> 'b)
  -> 'a
  -> 'b
