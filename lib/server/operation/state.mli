(** Update the state cache. *)
val update_state
  :  (module Sigs.EFFECT_HANDLER)
  -> Path.t
  -> Kohai_model.Log.t
  -> unit

val get
  :  ?body:string
  -> ?id:int
  -> (module Sigs.EFFECT_HANDLER)
  -> unit
  -> Kohai_model.State.t
