(** A very simple IO monad. *)

type 'a t = unit -> 'a

val return : 'a -> 'a t
val bind : 'a t -> ('a -> 'b t) -> 'b t
val map : ('a -> 'b) -> 'a t -> 'b t
val apply : ('a -> 'b) t -> 'a t -> 'b t
val zip : 'a t -> 'b t -> ('a * 'b) t

module Syntax : sig
  (** [let+ x = e in f x] is [f <$> x]*)
  val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t

  (** [let+ x = e and+ y = f in g x y] is [g <$> e <*> f]. *)
  val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t

  (** [let* x = e in f x] is [e >>= f]. *)
  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
end

include module type of Syntax (** @inline *)

val perform : 'a Effect.t -> 'a t
val run : ('a -> 'b) -> 'a t -> 'b
val force : 'a t -> 'a
val compose : ('a -> 'b) -> ('c -> 'a) -> 'c t -> 'b t
