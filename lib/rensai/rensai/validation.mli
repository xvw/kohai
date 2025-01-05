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

(** Describes a value passed through a validation phase. *)
type 'a checked = ('a, value_error) result

(** Describes a validation function. *)
type 'a t = Ast.t -> 'a checked

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
