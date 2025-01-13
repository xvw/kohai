(** Allows the description of precise validation schemes for data
    described in Rensai format. *)

open Prelude

(**/**)

(**  {@ocaml[
       # open Rensai ;;
       # #install_printer Rensai.Ast.pp ;;
     ]} *)

(**/**)

(** The main aim of the Validation module is to build validation
    sequences capable of arbitrarily validating data described by
    {!module:Rensai.Ast}. For example:

    Let's imagine the following expression:

    {@ocaml[
      # let a_person =
           let open Ast in
           record [
             "first_name", string "Xavier"
           ; "last_name", string "MyLastName"
           ; "age", int 35
           ]
        ;;
      val a_person : Ast.t =
        {age = 35; first_name = "Xavier"; last_name = "MyLastName"}
    ]}

    This type of representation can be validated as follows:

    {@ocaml[
      # let validate_person =
         let open Validation in
         record (fun fields ->
            let open Record in
            let+ first_name = required fields "first_name" string
            and+ last_name = required fields "last_name" string
            and+ age = optional fields "age" int
            in (first_name, last_name, age)
         ) ;;
      val validate_person : (string * string * int option) Validation.t = <fun>
    ]}

    And we can use it for a valid person:

    {@ocaml[
      # let x = validate_person a_person ;;
      val x : (string * string * int option) Validation.checked =
        Ok ("Xavier", "MyLastName", Some 35)
    ]}

    Or for an invalid person:

    {rr@ocaml[
      # let y = validate_person
           Ast.(record ["first_name", list int [1; 2; 3]]) ;;
      val y : (string * string * int option) Validation.checked =
        Error
         (Rensai.Validation.Unexpected_record
           {Rensai.Validation.errors = <abstr>; value = {first_name = [1; 2; 3]}})
    ]rr} *)

(** {1 Types} *)

(** Describes the type of possible validation errors. *)
type value_error =
  | Unexpected_kind of
      { expected : Kind.t
      ; given : Kind.t
      ; value : Ast.t
      }
  | Unexpected_pair of
      { error : pair_error
      ; value : Ast.t
      ; given : Kind.t
      }
  | Unexpected_list of
      { errors : (int * value_error) Nel.t
      ; value : Ast.t
      ; given : Kind.t
      }
  | Unexpected_record of
      { errors : record_error Nel.t
      ; value : Ast.t
      }
  | Unexpected_value of string

(** Dedicated error for Pair. *)
and pair_error =
  | Invalid_fst of value_error
  | Invalid_snd of value_error
  | Invalid_both of value_error * value_error

(** Dedicated error for Record. *)
and record_error =
  | Invalid_field of
      { field : string
      ; error : value_error
      }
  | Missing_field of string

(** Describes a value passed through a validation phase. *)
type 'a checked = ('a, value_error) result

(** Describes a record passed through a validation phase. *)
type 'a checked_record = ('a, record_error Nel.t) result

(** Describes a validation function. *)
type ('a, 'b) v = 'a -> 'b checked

(** Describes a validation function from an AST fragment. *)
type 'a t = (Ast.t, 'a) v

(** Describes a validation function for a record. *)
type 'a record_validator = (string * Ast.t) list -> 'a checked_record

(** {1 Combinators}

    A set of combiners for composing validators. *)

(** {2 Infix operators} *)

module Infix : sig
  (** [f <$> v] is an infix version of [Result.map f v]. *)
  val ( <$> ) : ('a -> 'b) -> 'a checked -> 'b checked

  (** [validator $ f] apply [f] (and wrap it into a success) on the
      result of [validator]. *)
  val ( $ ) : ('a, 'b) v -> ('b -> 'c) -> ('a, 'c) v

  (** [validator $ f] apply [f] on the result of [validator]. *)
  val ( & ) : ('a, 'b) v -> ('b, 'c) v -> ('a, 'c) v

  (** [validator1 / validator2] performs [validator2] if [validator1]
      fail. *)
  val ( / ) : ('a, 'b) v -> ('a, 'b) v -> ('a, 'b) v
end

include module type of Infix (** @inline *)

(** {2 Bindings operators} *)

module Syntax : sig
  (** [let+ x = y in f x] is [Result.map (fun x -> f x) y]. *)
  val ( let+ ) : 'a checked -> ('a -> 'b, 'b) v

  (** [let* x = y in f x] is [Result.bind y (fun x -> f x)]. *)
  val ( let* ) : 'a checked -> (('a, 'b) v, 'b) v

  (** [let+ a = x and+ b = y in f x y] create a tuple from [x] and
      [y]. *)
  val ( and+ ) : 'a checked -> 'b checked -> ('a * 'b) checked

  (** [let* a = x and* b = y in f x y] create a tuple from [x] and
      [y]. *)
  val ( and* ) : 'a checked -> 'b checked -> ('a * 'b) checked
end

include module type of Syntax (** @inline *)

(** {1 Validators}

    Set of “simple” validators that act on data fragments described as
    {!type:Rensai.Ast.t}. Some validators are relaxed (using
    [?strict]. By default, the value of [strict] is [false]) *)

(** {2 Null and unit} *)

(** [null] ensures that the fragment is a [Null]. *)
val null : unit t

(** [unit] ensures that the fragment is a [Unit]. *)
val unit : unit t

(** [unitish] accept [null] or [unit]. *)
val unitish : unit t

(** {2 Simples} *)

(** [bool] ensure that the fragment is a [bool]. (or a string if
    [strict] is [false]). *)
val bool : ?strict:bool -> bool t

(** [char] ensure that the fragment is a [char] (or a string of length
    [1] if [strict] is [false]). *)
val char : ?strict:bool -> char t

(** [string ?strict] ensure that the fragment is a [string]. If the
    flag [strict] is [false], the validation is relaxed accepting
    [bool] and [number]. *)
val string : ?strict:bool -> string t

(** {2 Numbers} *)

(** [int ?strict] ensure that the fragment is a [int]. If the flag
    [strict] is [false], the validation is relaxed accepting [int32],
    [int64], [float] or [string]. *)
val int : ?strict:bool -> int t

(** [int32 ?strict] ensure that the fragment is a [int32]. If the flag
    [strict] is [false], the validation is relaxed accepting [int],
    [int64], [float] or [string]. *)
val int32 : ?strict:bool -> int32 t

(** [int64 ?strict] ensure that the fragment is a [int64]. If the flag
    [strict] is [false], the validation is relaxed accepting [int],
    [int32], [float] or [string]. *)
val int64 : ?strict:bool -> int64 t

(** [float ?strict] ensure that the fragment is a [float]. If the flag
    [strict] is [false], the validation is relaxed accepting [int],
    [int32], [int64] or [string]. *)
val float : ?strict:bool -> float t

(** [integer ?strict] ensure that the fragment is an [integer]. If the flag
    [strict] is [false], the validation is relaxed accepting [string]. *)
val integer : ?strict:bool -> int64 t

(** [number ?strict] ensure that the fragment is an [number]. If the flag
    [strict] is [false], the validation is relaxed accepting [string]. *)
val number : ?strict:bool -> float t

(** {2 Product} *)

(** [pair v1 v2] use [v2] and [v2] to validate a [pair]. *)
val pair : 'a t -> 'b t -> ('a * 'b) t

(** [triple v1 v2 v3] is a shortcut for
    [pair v1 (pair v2 v3) $ fun (x, (y, z)) -> x, y, z]. *)
val triple : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t

(** [quad v1 v2 v3 v4] is a shortcut for
    [pair v1 (pair v2 (pair v3 v4)) $ fun (w, (x, (y, z))) -> w, x, y, z]. *)
val quad : 'a t -> 'b t -> 'c t -> 'd t -> ('a * 'b * 'c * 'd) t

(** {2 List} *)

(** [list] is a validator that extract a list (to perform manual validation). *)
val list : Ast.t list t

(** [list_of v] is a validator that extract a list of values validated
    by [v]. *)
val list_of : 'a t -> 'a list t

(** {2 Sum} *)

(** [sum list_of_ctor] is a validator for arbitrary sum types. If
    [strict] is [false] the validator also consider [pair] and [list]
    as potential sum types. *)
val sum : ?strict:bool -> (string * 'a t) list -> 'a t

(** [option v] is a validator for optional value validated by [v]. *)
val option : 'a t -> 'a option t

(** [either l r] is a validator for [Either.t]. *)
val either : ?strict:bool -> 'a t -> 'b t -> ('a, 'b) Either.t t

(** [result okv errv] is a validator for [result]. *)
val result : ?strict:bool -> 'a t -> 'b t -> ('a, 'b) result t

(** {2 Record} *)

val record : ?strict:bool -> 'a record_validator -> 'a t

module Record : sig
  (** Functions for validating records fields. *)

  (** [optional fields field_name validator] optional [field] in
      [fields] validated by [validator]. *)
  val optional
    :  (string * Ast.t) list
    -> string
    -> 'a t
    -> 'a option checked_record

  (** [required fields field_name validator] required [field] in
      [fields] validated by [validator]. *)
  val required : (string * Ast.t) list -> string -> 'a t -> 'a checked_record

  (** [optional_or ~default fields field_name validator] optional
      [field] in [fields] validated by [validator] or [default]. *)
  val optional_or
    :  default:'a
    -> (string * Ast.t) list
    -> string
    -> 'a t
    -> 'a checked_record

  (** {2 Bindings operators} *)

  module Syntax : sig
    (** [let+ x = y in f x] is [Result.map (fun x -> f x) y]. *)
    val ( let+ ) : 'a checked_record -> ('a -> 'b) -> 'b checked_record

    (** [let* x = y in f x] is [Result.bind y (fun x -> f x)]. *)
    val ( let* )
      :  'a checked_record
      -> ('a -> 'b checked_record)
      -> 'b checked_record

    (** [let+ a = x and+ b = y in f x y] create a tuple from [x] and
        [y]. *)
    val ( and+ )
      :  'a checked_record
      -> 'b checked_record
      -> ('a * 'b) checked_record

    (** [let* a = x and* b = y in f x y] create a tuple from [x] and
        [y]. *)
    val ( and* )
      :  'a checked_record
      -> 'b checked_record
      -> ('a * 'b) checked_record
  end

  include module type of Syntax (** @inline *)
end

(** {2 Generic validators} *)

(** [const k r] wrap [k] as valid and discard [r]. *)
val const : 'a -> ('b, 'a) v

(** [where ?pp ?message predicate x] ensure that [x] is satisfying
    [predicate]. [message] and [pp] are used for error-reporting. *)
val where
  :  ?pp:(Format.formatter -> 'a -> unit)
  -> ?message:((Format.formatter -> 'a -> unit) -> 'a -> string)
  -> ('a -> bool)
  -> ('a, 'a) v

(** [unless ?pp ?message predicate x] ensure that [x] is not satisfying
    [predicate]. [message] and [pp] are used for error-reporting. *)
val unless
  :  ?pp:(Format.formatter -> 'a -> unit)
  -> ?message:((Format.formatter -> 'a -> unit) -> 'a -> string)
  -> ('a -> bool)
  -> ('a, 'a) v

(** [refute ?pp ?message validator] invalid a validator. *)
val refute
  :  ?pp:(Format.formatter -> 'a -> unit)
  -> ?message:((Format.formatter -> 'a -> unit) -> 'a -> string)
  -> ('a, 'a) v
  -> ('a, 'a) v

(** [equal ?pp ?eq a b] ensure that [a] = [b]. *)
val equal
  :  ?pp:(Format.formatter -> 'a -> unit)
  -> ?eq:('a -> 'a -> bool)
  -> 'a
  -> ('a, 'a) v

(** [not_equal ?pp ?eq a b] ensure that [a] <> [b]. *)
val not_equal
  :  ?pp:(Format.formatter -> 'a -> unit)
  -> ?eq:('a -> 'a -> bool)
  -> 'a
  -> ('a, 'a) v

(** [greater ?pp ?compare ~than:a b] ensure that [a < b]. *)
val greater
  :  ?pp:(Format.formatter -> 'a -> unit)
  -> ?compare:('a -> 'a -> int)
  -> than:'a
  -> ('a, 'a) v

(** [greater ?pp ?compare ~than:a b] ensure that [a <= b]. *)
val greater_or_equal
  :  ?pp:(Format.formatter -> 'a -> unit)
  -> ?compare:('a -> 'a -> int)
  -> than:'a
  -> ('a, 'a) v

(** [less ?pp ?compare ~than:a b] ensure that [a > b]. *)
val less
  :  ?pp:(Format.formatter -> 'a -> unit)
  -> ?compare:('a -> 'a -> int)
  -> than:'a
  -> ('a, 'a) v

(** [less_or_equal ?pp ?compare ~than:a b] ensure that [a >= b]. *)
val less_or_equal
  :  ?pp:(Format.formatter -> 'a -> unit)
  -> ?compare:('a -> 'a -> int)
  -> than:'a
  -> ('a, 'a) v

(** [in_range ?pp ?compare ~min ~max a] ensure that [a >= min] and
    [a < max], [a <- [min; max]]. *)
val in_range
  :  ?pp:(Format.formatter -> 'a -> unit)
  -> ?compare:('a -> 'a -> int)
  -> min:'a
  -> max:'a
  -> ('a, 'a) v

(** [outside_range ?pp ?compare ~min ~max a] ensure that [a < min] or
    [a > max]. *)
val outside_range
  :  ?pp:(Format.formatter -> 'a -> unit)
  -> ?compare:('a -> 'a -> int)
  -> min:'a
  -> max:'a
  -> ('a, 'a) v

(** [one_of ?pp ?eq list x] ensure that [x] is present in [list]. *)
val one_of
  :  ?pp:(Format.formatter -> 'a -> unit)
  -> ?eq:('a -> 'a -> bool)
  -> 'a list
  -> ('a, 'a) v

(** {2 Specific validators} *)

(** Validators specific to int. *)
module Int :
  Sigs.COMPLETE_NUMBER_VALIDATOR
  with type t := int
   and type error := value_error

(** Validators specific to int32. *)
module Int32 :
  Sigs.COMPLETE_NUMBER_VALIDATOR
  with type t := int32
   and type error := value_error

(** Validators specific to int64. *)
module Int64 :
  Sigs.COMPLETE_NUMBER_VALIDATOR
  with type t := int64
   and type error := value_error

(** Validators specific to float. *)
module Float :
  Sigs.COMPLETE_NUMBER_VALIDATOR
  with type t := float
   and type error := value_error

(** Validators specific to char. *)
module Char : sig
  (** @inline *)
  include
    Sigs.COMPLETE_VALIDATOR with type t := char and type error := value_error

  (** [is_digit] ensure that the given char is a digit. *)
  val is_digit : (char, char) v

  (** [as_digit] if the given char is a digit, it lift it into an int. *)
  val as_digit : (char, int) v

  (** [is_digit] ensure that the given char is 'a' .. 'z' | 'A' .. 'Z'. *)
  val is_alpha : (char, char) v

  (** [is_lowercase] ensure that the given char is 'a' .. 'z'. *)
  val is_lowercase : (char, char) v

  (** [is_uppercase] ensure that the given char is 'A' .. 'Z'. *)
  val is_uppercase : (char, char) v

  (** [is_whitespace] ensure that the given char is a whitespace. *)
  val is_whitespace : (char, char) v

  (** [is_newline] ensure that the given char is a newline. *)
  val is_newline : (char, char) v

  (** [is_alpha] ensure that the given char is 'a' .. 'z' | 'A' .. 'Z'
      | is_digit. *)
  val is_alphanumeric : (char, char) v

  (** [is_hex_digit] ensure that the given char is an hexadecimal digit. *)
  val is_hex_digit : (char, char) v

  (** [as_hex_digit] if the given char is an hex digit, it lift it
      into an int. *)
  val as_hex_digit : (char, int) v
end

(** Validators specific to boolean. *)
module Bool : sig
  (** @inline *)
  include
    Sigs.COMPLETE_VALIDATOR with type t := bool and type error := value_error

  (** [is_true] ensure that the given value is [true]. *)
  val is_true : (bool, bool) v

  (** [is_false] ensure that the given value is [false]. *)
  val is_false : (bool, bool) v

  (** [negate] negate the given value. *)
  val negate : (bool, bool) v
end

(** Validators specific to string. *)
module String : sig
  (** @inline *)
  include
    Sigs.COMPLETE_VALIDATOR with type t := string and type error := value_error

  (** [trim] trim the given string. *)
  val trim : (string, string) v

  (** [downcase] downcase the given string. *)
  val downcase : (string, string) v

  (** [upcase] upcase the given string. *)
  val upcase : (string, string) v

  (** [capitalize] capitalize the given string. *)
  val capitalize : (string, string) v

  (** [uncapitalize] uncapitalize the given string. *)
  val uncapitalize : (string, string) v

  (** [is_empty] ensure the given string is empty. *)
  val is_empty : (string, string) v

  (** [is_not_empty] ensure the given string is not empty. *)
  val is_not_empty : (string, string) v

  (** [is_blank] ensure the given string is blank. *)
  val is_blank : (string, string) v

  (** [is_not_blank] ensure the given string is not blank. *)
  val is_not_blank : (string, string) v

  (** [start_with prefix] ensure that the given string start with [prefix]. *)
  val start_with : string -> (string, string) v

  (** [ends_with suffix] ensure that the given string ends with [suffix]. *)
  val ends_with : string -> (string, string) v

  (** [is_slug ?accept_capital ?unknown ?separator] ensure that a string
      look like a slug.*)
  val is_slug
    :  ?accept_capital:bool
    -> ?unknown:char
    -> ?separator:char
    -> (string, string) v
end

(** Validators specific to list. *)
module List : sig
  (** [where ?pp ?message predicate x] ensure that [x] is satisfying
      [predicate]. [message] and [pp] are used for error-reporting. *)
  val where
    :  ?pp:(Format.formatter -> 'a -> unit)
    -> ?message:((Format.formatter -> 'a list -> unit) -> 'a list -> string)
    -> ('a list -> bool)
    -> ('a list, 'a list) v

  (** [unless ?pp ?message predicate x] ensure that [x] is not satisfying
      [predicate]. [message] and [pp] are used for error-reporting. *)
  val unless
    :  ?pp:(Format.formatter -> 'a -> unit)
    -> ?message:((Format.formatter -> 'a list -> unit) -> 'a list -> string)
    -> ('a list -> bool)
    -> ('a list, 'a list) v

  (** [refute ?pp ?message validator] invalid a validator. *)
  val refute
    :  ?pp:(Format.formatter -> 'a -> unit)
    -> ?message:((Format.formatter -> 'a list -> unit) -> 'a list -> string)
    -> ('a list, 'a list) v
    -> ('a list, 'a list) v

  (** [is_empty ?pp] ensure that the given list is empty. *)
  val is_empty : ?pp:(Format.formatter -> 'a -> unit) -> ('a list, 'a list) v

  (** [is_not_empty ?pp] ensure that the given list is not empty. *)
  val is_not_empty
    :  ?pp:(Format.formatter -> 'a -> unit)
    -> ('a list, 'a list) v

  (** [as_nel ?pp] ensure that the given list is a non-empty list. *)
  val as_nel : ('a list, 'a Nel.t) v

  (** [has_length l] ensure that the given list has the length [l]. *)
  val has_length
    :  ?pp:(Format.formatter -> 'a -> unit)
    -> int
    -> ('a list, 'a list) v

  val for_all
    :  ?pp:(Format.formatter -> 'a -> unit)
    -> ('a -> bool)
    -> ('a list, 'a list) v
end

(** {1 Misc} *)

(** Pretty Printers for value error. *)
val pp_value_error : Format.formatter -> value_error -> unit

(** Pretty Printers for checked values. *)
val pp_checked
  :  (Format.formatter -> 'a -> unit)
  -> Format.formatter
  -> 'a checked
  -> unit
