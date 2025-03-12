(** Promote a transient log into a Regular one. *)
val promote
  :  (module Sigs.EFFECT_HANDLER)
  -> Path.t
  -> Kohai_model.Log.t
  -> unit

(** Promote a log into a transient one. *)
val unpromote
  :  (module Sigs.EFFECT_HANDLER)
  -> Path.t
  -> Kohai_model.Log.t
  -> unit

(** Get a log by ID.*)
val get
  :  (module Sigs.EFFECT_HANDLER)
  -> Uuid.t
  -> (Datetime.t * Kohai_model.Log.t) option

(** Get the last log. *)
val get_last
  :  (module Sigs.EFFECT_HANDLER)
  -> unit
  -> Datetime.t * Kohai_model.Log.t list

(** Get the last log for a sector. *)
val get_last_for_sector
  :  (module Sigs.EFFECT_HANDLER)
  -> string
  -> Datetime.t * Kohai_model.Log.t list

(** Get the last log for a project. *)
val get_last_for_project
  :  (module Sigs.EFFECT_HANDLER)
  -> string
  -> Datetime.t * Kohai_model.Log.t list
