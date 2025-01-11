(** Some shared interfaces *)

(** {1 Inputs} *)

module type DUMPABLE = sig
  type t

  val pp : Format.formatter -> t -> unit
end

module type EQUATABLE = sig
  include DUMPABLE

  val equal : t -> t -> bool
end

module type COMPARABLE = sig
  include DUMPABLE

  val compare : t -> t -> int
end

module type NUMBER = sig
  include COMPARABLE

  val one : t
  val zero : t
end

(** {1 Outputs} *)

module type SIMPLE_VALIDATOR = sig
  type t
  type error

  (** [where ?message predicate x] ensure that [x] is satisfying
      [predicate]. [message] is used for error-reporting. *)
  val where
    :  ?message:((Format.formatter -> t -> unit) -> t -> string)
    -> (t -> bool)
    -> t
    -> (t, error) result

  (** [unless ?message predicate x] ensure that [x] is not satisfying
      [predicate]. [message] is used for error-reporting. *)
  val unless
    :  ?message:((Format.formatter -> t -> unit) -> t -> string)
    -> (t -> bool)
    -> t
    -> (t, error) result

  (** [refute ?message validator] invalid a validator. *)
  val refute
    :  ?message:((Format.formatter -> t -> unit) -> t -> string)
    -> (t -> (t, error) result)
    -> t
    -> (t, error) result
end

module type EQUATABLE_VALIDATOR = sig
  type t
  type error

  (** [equal a b] ensure that [a] = [b]. *)
  val equal : t -> t -> (t, error) result

  (** [not_equal a b] ensure that [a] <> [b]. *)
  val not_equal : t -> t -> (t, error) result

  (** [one_of list x] ensure that [x] is present in [list]. *)
  val one_of : t list -> t -> (t, error) result
end

module type COMPARABLE_VALIDATOR = sig
  type t
  type error

  (** [greater ~than:a b] ensure that [a < b]. *)
  val greater : than:t -> t -> (t, error) result

  (** [greater_or_equal ~than:a b] ensure that [a <= b]. *)
  val greater_or_equal : than:t -> t -> (t, error) result

  (** [less ~than:a b] ensure that [a > b]. *)
  val less : than:t -> t -> (t, error) result

  (** [less_or_equal ~than:a b] ensure that [a >= b]. *)
  val less_or_equal : than:t -> t -> (t, error) result

  (** [in_range ~min ~max a] ensure that
      [a >= min] and [a < max], [a <- [min; max]]. *)
  val in_range : min:t -> max:t -> t -> (t, error) result

  (** [outside_range ~min ~max a] ensure that
      [a < min] and [a > max], [a <- [min; max]]. *)
  val outside_range : min:t -> max:t -> t -> (t, error) result
end

module type NUMBER_VALIDATOR = sig
  type t
  type error

  (** [is_null x] ensure that [x] is null. *)
  val is_null : t -> (t, error) result

  (** [is_null x] ensure that [x] is not null. *)
  val is_not_null : t -> (t, error) result

  (** [is_null x] ensure that [x] is positive. *)
  val is_positive : t -> (t, error) result

  (** [is_null x] ensure that [x] is negative. *)
  val is_negative : t -> (t, error) result
end

module type COMPLETE_NUMBER_VALIDATOR = sig
  type t
  type error

  (** @inline *)
  include SIMPLE_VALIDATOR with type t := t and type error := error

  (** @inline *)
  include EQUATABLE_VALIDATOR with type t := t and type error := error

  (** @inline *)
  include COMPARABLE_VALIDATOR with type t := t and type error := error

  (** @inline *)
  include NUMBER_VALIDATOR with type t := t and type error := error
end
