(** Promote a transient log into a Regular one. *)
val promote
  :  ?body:string
  -> ?id:int
  -> (module Sigs.EFFECT_HANDLER)
  -> Path.t
  -> Kohai_model.Log.t
  -> unit

(** Promote a log into a transient one. *)
val unpromote
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
  -> (Datetime.t * Kohai_model.Log.t) option

(** Get the last log. *)
val get_last
  :  ?body:string
  -> ?id:int
  -> (module Sigs.EFFECT_HANDLER)
  -> unit
  -> Datetime.t * Kohai_model.Log.t list

(** Get the last log for a sector. *)
val get_last_for_sector
  :  ?body:string
  -> ?id:int
  -> (module Sigs.EFFECT_HANDLER)
  -> string
  -> Datetime.t * Kohai_model.Log.t list

(** Get the last log for a project. *)
val get_last_for_project
  :  ?body:string
  -> ?id:int
  -> (module Sigs.EFFECT_HANDLER)
  -> string
  -> Datetime.t * Kohai_model.Log.t list
