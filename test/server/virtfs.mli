(** A virtual (and artificial) file system mainly used to describe
    unit tests. Since the main purpose is mostly testing, the code is
    not particularly optimized. *)

open Kohai_core

(** {1 Types} *)

(** An item in the filesystem. *)
type item

(** A file system representation. *)
type t

(** {1 Build filesystem} *)

(** [file ?mtime ?content name] create a virtual file. *)
val file : ?mtime:int -> ?content:string -> string -> item

(** [dir ?mtime name children] create a virtual directory. *)
val dir : ?mtime:int -> string -> item list -> item

(** [from_list ?mtime elt] create a file system from a list. ([mtime]
    is used for the root of the file tree) *)
val from_list : ?mtime:int -> item list -> t

(** {1 Interacting with filesystem} *)

(** [get fs path] try to reach a path. *)
val get : t -> Path.t -> item option

(** [cat fs path] a very naive way to read a file (it is a little bit
    like [Unix cat] but that does not concatenate file because
    hehe). *)
val cat : t -> Path.t -> string

val update
  :  t
  -> Path.t
  -> (target:string -> ?previous:item -> unit -> item option)
  -> t

(** {1 Effect handler} *)

module Make (_ : sig
    val fs : t
    val now : Datetime.t
  end) : sig
  include Sigs.EFFECT_REQUIREMENT

  val get_fs : unit -> t
  val manip_time : (Datetime.t -> Datetime.t) -> unit
end
