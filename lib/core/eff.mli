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

module Handler (_ : Sigs.EFFECT_REQUIREMENT) : Sigs.EFFECT_HANDLER

(** {1 Public API}

    However, even when using a functor, the aim is essentially to use
    functions that take handlers as arguments. This is generally
    possible because a handler does not introduce parameterized types. *)

(** [handle program] Interprets the [program] with the given
    handler.*)
val handle
  :  (module Sigs.EFFECT_HANDLER)
  -> ((module Sigs.EFFECT_HANDLER) -> unit -> 'a)
  -> ('a, Sigs.jsonrpc_error) result
