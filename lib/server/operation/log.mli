(** Promote a transient log into a Regular one. *)
val promote
  :  ?body:string
  -> ?id:int
  -> (module Sigs.EFFECT_HANDLER)
  -> Path.t
  -> Kohai_model.Log.t
  -> unit
