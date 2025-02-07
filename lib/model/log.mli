(** The logs are a collection of time-ordered notes on how I spent my
    free time on various and sundry projects. *)

module Transient : sig
  (** Transient logs are logs that need to be classified. Each time a
      log is entered, it is first put into a transient state. *)

  (** A type that denotes a transient log. *)
  type t

  (** Convert transient log to rensai lang. *)
  val to_rensai : t Rensai.Ast.conv

  (** Convert rensai expression to transient log. *)
  val from_rensai : t Rensai.Validation.t
end
