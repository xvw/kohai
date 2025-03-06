(** Operation between transient logs and regular logs. *)

(** Process the action of unpromote a log. *)
val unpromote_log
  :  ?id:int
  -> body:string
  -> (module Sigs.EFFECT_HANDLER)
  -> Uuid.t
  -> Datetime.t * Kohai_model.Transient_log.t list
