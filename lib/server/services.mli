(** Set of services (methods) supported by the JSONRPC server. *)

(** A shortcut describing a pair of method name and controller. *)
type t = string * Jsonrpc.service

(** List of available methods of the server. *)
val all : string -> t list
