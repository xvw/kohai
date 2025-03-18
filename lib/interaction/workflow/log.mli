(** Get a log by UUID. *)
val get : (module Sigs.EFFECT_HANDLER) -> Uuid.t -> Kohai_model.Log.t option

(** Return the list of last log. *)
val last : (module Sigs.EFFECT_HANDLER) -> unit -> Kohai_model.Log.t list

(** Return the list of last log for a given sector. *)
val last_for_sector
  :  (module Sigs.EFFECT_HANDLER)
  -> string
  -> Kohai_model.Log.t list

(** Return the list of last log for a given project. *)
val last_for_project
  :  (module Sigs.EFFECT_HANDLER)
  -> string
  -> Kohai_model.Log.t list

(** Unpromote a log to a transient log *)
val unpromote
  :  (module Sigs.EFFECT_HANDLER)
  -> Uuid.t
  -> Kohai_model.Transient_log.t list
