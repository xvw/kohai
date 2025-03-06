(** Update the state cache. *)
val upgrade
  :  ?id:int
  -> body:string
  -> (module Sigs.EFFECT_HANDLER)
  -> Path.t
  -> Kohai_model.Log.t
  -> unit

val downgrade
  :  ?id:int
  -> body:string
  -> (module Sigs.EFFECT_HANDLER)
  -> Path.t
  -> Kohai_model.Log.t
  -> unit

val get
  :  ?id:int
  -> body:string
  -> (module Sigs.EFFECT_HANDLER)
  -> unit
  -> Kohai_model.State.t

val get_for_sector
  :  ?id:int
  -> body:string
  -> (module Sigs.EFFECT_HANDLER)
  -> string
  -> Kohai_model.State.t

val get_for_project
  :  ?id:int
  -> body:string
  -> (module Sigs.EFFECT_HANDLER)
  -> string
  -> Kohai_model.State.t
