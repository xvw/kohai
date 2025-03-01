(** Promote a transient log into a Regular one. *)
val promote
  :  ?body:string
  -> ?id:int
  -> (module Sigs.EFFECT_HANDLER)
  -> Path.t
  -> Kohai_model.Log.t
  -> unit

(** Get a log by ID.*)
val get
  :  ?body:string
  -> ?id:int
  -> (module Sigs.EFFECT_HANDLER)
  -> Uuid.t
  -> Kohai_model.Log.t option
