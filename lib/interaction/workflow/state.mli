(** Get the global state. *)
val get : (module Sigs.EFFECT_HANDLER) -> unit -> Kohai_model.State.t

(** Get state for a given sector *)
val get_for_sector
  :  (module Sigs.EFFECT_HANDLER)
  -> string
  -> Kohai_model.State.t

(** Get state for a given project *)
val get_for_project
  :  (module Sigs.EFFECT_HANDLER)
  -> string
  -> Kohai_model.State.t
