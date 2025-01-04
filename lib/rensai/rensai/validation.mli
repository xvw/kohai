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
    {!type:Rensai.Ast.t}. *)

(** [null] ensures that the fragment is a [Null]. *)
val null : unit t

(** [unit] ensures that the fragment is a [Unit]. *)
val unit : unit t

(** [unitish] accept [null] or [unit]. *)
val unitish : unit t

(** [bool] ensure that the fragment is a [bool]. *)
val bool : bool t
