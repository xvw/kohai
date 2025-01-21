(** A virtual (and artificial) file system mainly used to describe
    unit tests. *)

open Kohai_core

(** {1 Types} *)

(** An item in the filesystem. *)
type item

(** A file system representation. *)
type t

(** {1 Utils} *)

val get : t -> Path.t -> item option
