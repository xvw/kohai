(** Set of actions to describe server methods *)

val get_transient_log
  :  string
  -> ?id:int
  -> (module Sigs.EFFECT_HANDLER)
  -> unit
  -> Kohai_model.Log.Transient.t list

val record_log
  :  string
  -> ?id:int
  -> (module Sigs.EFFECT_HANDLER)
  -> Kohai_model.Log.Recored.t
  -> Kohai_model.Log.Transient.result

val stop_recording
  :  string
  -> ?id:int
  -> (module Sigs.EFFECT_HANDLER)
  -> Kohai_model.Log.Transient.Operate.Stop.t
  -> Kohai_model.Log.Transient.t list

val rewrite_transient_log
  :  string
  -> ?id:int
  -> (module Sigs.EFFECT_HANDLER)
  -> Kohai_model.Log.Transient.t
  -> Kohai_model.Log.Transient.t list
