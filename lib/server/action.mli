(** Set of actions to describe server methods *)

val ensure_supervision
  :  string
  -> ?id:int
  -> (module Sigs.EFFECT_HANDLER)
  -> unit
  -> unit

val with_supervision
  :  string
  -> ?id:int
  -> (module Sigs.EFFECT_HANDLER)
  -> (string -> ?id:int -> (module Sigs.EFFECT_HANDLER) -> 'a -> 'b)
  -> 'a
  -> 'b

val get_supervised_directory
  :  ?id:int
  -> (module Sigs.EFFECT_HANDLER)
  -> unit
  -> Path.t option

val set_supervised_directory
  :  string
  -> ?id:int
  -> (module Sigs.EFFECT_HANDLER)
  -> Path.t
  -> unit
