(** Operation related to transient logs. *)

val all : (module Sigs.EFFECT_HANDLER) -> Kohai_model.Transient_log.t list
