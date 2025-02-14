(** Set of actions to describe server methods *)

val ensure_supervision
  :  string
  -> ?id:int
  -> (module Sigs.EFFECT_HANDLER)
  -> unit
  -> Kohai_core.Path.t

val with_supervision
  :  string
  -> ?id:int
  -> (module Sigs.EFFECT_HANDLER)
  -> (string -> ?id:int -> (module Sigs.EFFECT_HANDLER) -> 'a -> 'b)
  -> 'a
  -> 'b

val get_supervised_directory
  :  ?id:int
  -> (module Sigs.EFFECT_HANDLER)
  -> unit
  -> Path.t option

val is_valid_supervised_directory
  :  ?id:int
  -> (module Sigs.EFFECT_HANDLER)
  -> Path.t
  -> bool

val set_supervised_directory
  :  string
  -> ?id:int
  -> (module Sigs.EFFECT_HANDLER)
  -> Path.t
  -> unit

val get_sectors
  :  string
  -> ?id:int
  -> (module Sigs.EFFECT_HANDLER)
  -> unit
  -> Kohai_model.Sector.Set.t

val save_sector
  :  string
  -> ?id:int
  -> (module Sigs.EFFECT_HANDLER)
  -> Kohai_model.Sector.t
  -> Kohai_model.Sector.Set.t

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
