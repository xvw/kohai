(** The context is built into a request and sent to the controller and
    a service finalizer. *)

type t

val make : now:Datetime.t -> t
val now : t -> Datetime.t
