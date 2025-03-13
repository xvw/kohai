(** The supervised directory is where log artifacts are stored. *)

(** Defines a new supervised directory. *)
val set : (module Sigs.EFFECT_HANDLER) -> Path.t -> Path.t

(** Return the supervised directory for the current session. *)
val get : (module Sigs.EFFECT_HANDLER) -> unit -> Path.t option

(** Return [true] if a path is valid for supervision, [false]
    otherwise. *)
val is_valid : (module Sigs.EFFECT_HANDLER) -> Path.t -> bool

(** Ensure that an action is guarded by a supervised directory. *)
val ensure : (module Sigs.EFFECT_HANDLER) -> unit -> Path.t
