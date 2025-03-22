(** Describes an application state (essentially for caching). *)

type t

(** Create a new cache environment. *)
val big_bang : unit -> t

(** Create a state. *)
val make
  :  ?big_bang:Datetime.t
  -> ?end_of_world:Datetime.t
  -> ?number_of_logs:int
  -> ?duration:Duration.t
  -> unit
  -> t

(** Update date boundaries of the cache. *)
val patch_date_boundaries : Datetime.t -> t -> t

val increase_duration : Duration.t -> t -> t
val decrease_duration : Duration.t -> t -> t
val increase_counter : int -> t -> t
val decrease_counter : int -> t -> t

(** {1 Serialization} *)

val from_rensai : t Rensai.Validation.t
val to_compact_rensai : t Rensai.Ast.conv
val from_string : string -> t
val dump : t -> string

(** {1 Extract information} *)

val big_bang_of : t -> Datetime.t option
val end_of_world_of : t -> Datetime.t option
val number_of_logs_of : t -> int
val duration_of : t -> Duration.t
