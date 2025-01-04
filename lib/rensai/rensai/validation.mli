type value_error =
  | Unexpected_kind of
      { expected : Kind.t
      ; given : Kind.t
      ; value : Ast.t
      }

type 'a checked = ('a, value_error) result
type 'a t = Ast.t -> 'a checked

val null : unit t
val unit : unit t
