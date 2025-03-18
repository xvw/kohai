(** Get a current log by index. *)
val get
  :  (module Sigs.EFFECT_HANDLER)
  -> int
  -> Kohai_model.Transient_log.t option

(** List all current transient logs. *)
val list
  :  (module Sigs.EFFECT_HANDLER)
  -> unit
  -> Kohai_model.Transient_log.t list

(** Perform update on transient log. *)
val action
  :  (module Sigs.EFFECT_HANDLER)
  -> Kohai_model.Context.t
  -> Kohai_model.Transient_log.operation
  -> Kohai_model.Transient_log.result
