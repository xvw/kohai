(** Set of actions to describe server methods *)

val ensure_supervision
  :  string
  -> ?id:int
  -> (module Sigs.EFFECT_HANDLER)
  -> unit
  -> unit
