(** A rather naive way of dealing with durations (The under the hood
    representation is described using integers (64) representing
    seconds). *)

(** {1 Types} *)

(** A duration (as a number of seconds). *)
type t

(** A duration splitted by days, hours, mins and sec. *)
type representation =
  { d : t
  ; h : t
  ; m : t
  ; s : t
  }

(** {1 From regular number} *)

val from_int : int -> t
val from_int32 : int32 -> t
val from_float : float -> t
val from_int64 : int64 -> t

(** {1 To regular number} *)

val to_int : t -> int
val to_int32 : t -> int32
val to_int64 : t -> int64
val to_float : t -> float

(** {1 Tools} *)

val zero : t
val add : t -> t -> t
val sub : t -> t -> t
val bound_positive : t -> t
val compute : t -> representation

(** {1 Misc} *)

(** Lift a duration into a rensai expression. *)
val to_rensai : t Rensai.Ast.conv

(** Pretty-printer for displaying a date from a representation. *)
val pp : Format.formatter -> t -> unit

(** Comparison between duration. *)
val compare : t -> t -> int
