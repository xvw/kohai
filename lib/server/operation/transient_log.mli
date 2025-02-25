(** Operation related to transient logs. *)

(** List all current transient logs. *)
val list
  :  ?body:string
  -> ?id:int
  -> (module Sigs.EFFECT_HANDLER)
  -> unit
  -> Datetime.t * Kohai_model.Transient_log.t list

(** Perform update on transient log. *)
val action
  :  ?body:string
  -> ?id:int
  -> (module Sigs.EFFECT_HANDLER)
  -> Kohai_model.Transient_log.operation
  -> Datetime.t * Kohai_model.Transient_log.result

(** Get a current log by index. *)
val get
  :  ?body:string
  -> ?id:int
  -> (module Sigs.EFFECT_HANDLER)
  -> int
  -> Kohai_model.Transient_log.t option

(** Promote a transient log into a real one. *)
val promote
  :  ?body:string
  -> ?id:int
  -> (module Sigs.EFFECT_HANDLER)
  -> int
  -> Datetime.t * Kohai_model.Transient_log.result
