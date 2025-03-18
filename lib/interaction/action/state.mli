(** Upgrade the state when a new log is promotted. *)
val upgrade
  :  Path.t
  -> (module Sigs.EFFECT_HANDLER)
  -> Kohai_model.Log.t
  -> unit

(** Downgrade the state when a new log is promotted. *)
val downgrade
  :  Path.t
  -> (module Sigs.EFFECT_HANDLER)
  -> Kohai_model.Log.t
  -> unit
