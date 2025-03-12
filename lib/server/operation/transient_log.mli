(** Operation related to transient logs. *)

(** Store missing data (project and sector). *)
val store_missing_data
  :  (module Sigs.EFFECT_HANDLER)
  -> sector:string
  -> project:string option
  -> unit

(** List all current transient logs. *)
val list
  :  (module Sigs.EFFECT_HANDLER)
  -> unit
  -> Datetime.t * Kohai_model.Transient_log.t list

(** Perform update on transient log. *)
val action
  :  (module Sigs.EFFECT_HANDLER)
  -> Kohai_model.Transient_log.operation
  -> Datetime.t * Kohai_model.Transient_log.result

val save
  :  (module Sigs.EFFECT_HANDLER)
  -> Path.t
  -> Datetime.t
  -> Kohai_model.Transient_log.t
  -> Datetime.t * Kohai_model.Transient_log.result

(** Get a current log by index. *)
val get
  :  (module Sigs.EFFECT_HANDLER)
  -> int
  -> Kohai_model.Transient_log.t option
