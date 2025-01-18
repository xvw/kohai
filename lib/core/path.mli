(** A (pure) abstraction to represent file paths. (Very largely inspired by
    {{:https://github.com/xhtmlboi/yocaml} YOCaml}!) *)

(** {1 Types} *)

(** The type describing a Path. *)
type t

(** {1 Building Path} *)

(** Return the current path. *)
val pwd : t

(** Return the root path. *)
val root : t

(** [from x ~into:s] build a path of [s] into [x]. *)
val from : t -> into:string -> t

(** {1 Path Manipulation} *)

(** [extension path] return the extension of the given [path] (and
    an empty string if the path does not have any extension). *)
val extension : t -> string

(** [extension_opt] is [extension] but wrap the result into an option. *)
val extension_opt : t -> string option

(** {1 Infix} *)

module Infix : sig
  (** [p / s] is [from p ~into:s]. *)
  val ( / ) : t -> string -> t

  (** [~/s] is [from pwd ~into:s]. *)
  val ( ~/ ) : string -> t
end

include module type of Infix (** @inline *)

(** {1 Misc} *)

(** Equality Between Path. *)
val equal : t -> t -> bool

(** Return the representation (in string) of a Path. *)
val to_string : t -> string

val from_rensai : t Rensai.Validation.t
val is_relative : t -> bool
val is_absolute : t -> bool
