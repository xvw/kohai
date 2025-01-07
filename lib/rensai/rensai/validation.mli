(** Allows the description of precise validation schemes for data
    described in Rensai format. *)

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

(** Dedicated error for Pair. *)
and pair_error =
  | Invalid_fst of value_error
  | Invalid_snd of value_error
  | Invalid_both of value_error * value_error

(** Describes a value passed through a validation phase. *)
type 'a checked = ('a, value_error) result

(** Describes a validation function. *)
type ('a, 'b) v = 'a -> 'b checked

(** Describes a validation function from an AST fragment. *)
type 'a t = (Ast.t, 'a) v

(** {1 Combinators}

    A set of combiners for composing validators. *)

(** {2 Infix operators} *)

module Infix : sig
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

(** {2 Infix operators} *)

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

(** [null] ensures that the fragment is a [Null]. *)
val null : unit t

(** [unit] ensures that the fragment is a [Unit]. *)
val unit : unit t

(** [unitish] accept [null] or [unit]. *)
val unitish : unit t

(** [bool] ensure that the fragment is a [bool]. (or a string if
    [strict] is [false]). *)
val bool : ?strict:bool -> bool t

(** [char] ensure that the fragment is a [char] (or a string of length
    [1] if [strict] is [false]). *)
val char : ?strict:bool -> char t

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

(** [string ?strict] ensure that the fragment is a [string]. If the
    flag [strict] is [false], the validation is relaxed accepting
    [bool] and [number]. *)
val string : ?strict:bool -> string t

(** [pair v1 v2] use [v2] and [v2] to validate a [pair]. *)
val pair : 'a t -> 'b t -> ('a * 'b) t

(** [triple v1 v2 v3] is a shortcut for
    [pair v1 (pair v2 v3) $ fun (x, (y, z)) -> x, y, z]. *)
val triple : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t

(** [quad v1 v2 v3 v4] is a shortcut for
    [pair v1 (pair v2 (pair v3 v4)) $ fun (w, (x, (y, z))) -> w, x, y, z]. *)
val quad : 'a t -> 'b t -> 'c t -> 'd t -> ('a * 'b * 'c * 'd) t

(** [list] is a validator that extract a list (to perform manual validation). *)
val list : Ast.t list t

(* val list_of : 'a t -> 'a list t *)
