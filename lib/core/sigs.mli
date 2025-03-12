(** Some shared interfaces *)

(** Describes all the effects a program can propagate. This module
    serves as a requirement for building a Handler. *)
module type EFFECT_REQUIREMENT = sig
  (** {1 Filesystem function} *)

  val exists : Path.t -> bool
  val is_file : Path.t -> bool
  val is_dir : Path.t -> bool
  val create_dir : Path.t -> unit
  val read_file : Path.t -> string
  val write_file : Path.t -> string -> unit
  val append_to_file : Path.t -> string -> unit
  val delete_file : Path.t -> unit
  val delete_dir : ?recursive:bool -> Path.t -> unit

  (** {1 Time function} *)

  val now : unit -> float
  val datetime_from_float : float -> Datetime.t Rensai.Validation.checked

  (** {1 Specific function} *)

  val set_supervised_directory : Path.t option -> unit
  val get_supervised_directory : unit -> Path.t option
end

(** An effect handler is built around a
    {!module-type:EFFECT_REQUIREMENT} to propagate platform-specific
    operations.*)
module type EFFECT_HANDLER = sig
  include EFFECT_REQUIREMENT

  exception Handler_exn of Error.custom

  (** [raise error] throws a fixed-error. *)
  val raise : Error.custom -> 'a

  (** [handle_with_error program], handle [program ()] with
      exception. *)
  val handle_with_error : (unit -> 'a) -> ('a, Error.custom) result
end
