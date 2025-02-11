(** The logs are a collection of time-ordered notes on how I spent my
    free time on various and sundry projects. *)

module Recored : sig
  type t

  val from_rensai : t Rensai.Validation.t
  val start_date_of : t -> Datetime.t option
  val sector_of : t -> string
end

module Transient : sig
  (** Transient logs are logs that need to be classified. Each time a
      log is entered, it is first put into a transient state. *)

  (** A type that denotes a transient log. *)
  type t

  (** Build a transient log. *)
  val make : start_date:Datetime.t -> Recored.t -> t

  (** Convert transient log to rensai lang. *)
  val to_rensai : t Rensai.Ast.conv

  (** Convert rensai expression to transient log. *)
  val from_rensai : t Rensai.Validation.t

  (** Compute the duration of a log given a specific date only if the
      specific date is after and if the duration was not already
      filled. *)
  val compute_duration : t -> Datetime.t -> t
end
