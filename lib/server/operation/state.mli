(** Update the state cache. *)
val upgrade
  :  (module Sigs.EFFECT_HANDLER)
  -> Path.t
  -> Kohai_model.Log.t
  -> unit

val downgrade
  :  (module Sigs.EFFECT_HANDLER)
  -> Path.t
  -> Kohai_model.Log.t
  -> unit

val get : (module Sigs.EFFECT_HANDLER) -> unit -> Kohai_model.State.t

val get_for_sector
  :  (module Sigs.EFFECT_HANDLER)
  -> string
  -> Kohai_model.State.t

val get_for_project
  :  (module Sigs.EFFECT_HANDLER)
  -> string
  -> Kohai_model.State.t
