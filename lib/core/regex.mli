(** Some presaved Regexp. *)

val trim : Re.t -> Re.t
val constant : string -> Re.t
val time_sep : Re.t
val min_or_sec : Re.t
val hour : Re.t
val at : Re.t
val time_full : Re.t
val opt_int_of_group : Re.Group.t -> int -> int option
