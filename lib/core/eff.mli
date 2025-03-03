(** This module offers a rather naive approach to abstracting the
    effects that can be envisaged in a program interpreted by the
    JSONRPC request server.

    There are several ways of describing effects abstraction in OCaml,
    and the most obvious is, of course, the use of User Defined
    Effects.{{:https://ocaml.org/manual/5.3/effects.html} See OCaml
    Manual}. But after several experiments, I found that I had
    relatively little need (if any) to explicitly control the
    continuation of the program. In fact, the only time I didn't need
    to control continuation was when dealing with errors. As a result,
    I prefer to use a {i more predictable} approach: modules, and only
    handle error cases with exceptions (user-defined-effects without
    continuation control).

    The main reason I wanted to abstract the effects was to make unit
    testing easier, and to make it easier to use the code in the
    Browser. *)

(** {1 Handler definition}

    A Handler is described using a functor. *)

module type HANDLER = Sigs.EFFECT_HANDLER

(** A shortcut to define function that should be handled. *)
type handler = (module HANDLER)

(** Build an handler on top of a set of requirement. *)
module Handler (_ : Sigs.EFFECT_REQUIREMENT) : HANDLER

(** {1 Public API}

    However, even when using a functor, the aim is essentially to use
    functions that take handlers as arguments. This is generally
    possible because a handler does not introduce parameterized types. *)

(** [raise (module Handler) error] throws [error] as an exception. *)
val raise : handler -> Sigs.jsonrpc_error -> 'a

(** Set the working directory of the session. *)
val set_supervised_directory : handler -> Path.t option -> unit

(** Get the working directory of the session. *)
val get_supervised_directory : handler -> Path.t option

(** [from_result (module Handler) callback res] handle error using
    effect from a result. *)
val from_result : handler -> ('b -> Sigs.jsonrpc_error) -> ('a, 'b) result -> 'a

(** {2 File management} *)

val exists : handler -> Path.t -> bool
val is_file : handler -> Path.t -> bool
val is_dir : handler -> Path.t -> bool
val read_file : handler -> Path.t -> string
val create_dir : handler -> Path.t -> unit
val write_file : handler -> Path.t -> string -> unit
val append_to_file : handler -> Path.t -> string -> unit
val delete : handler -> Path.t -> unit

(** {2 Time management} *)

val now : handler -> Datetime.t

(** {1 Program Handler} *)

(** [handle (module Handler) program] Interprets the [program] with the given
    handler.*)
val handle : handler -> (handler -> 'a) -> ('a, Sigs.jsonrpc_error) result
