(** Represents a stored log (non-transient). *)

(** Type describing a log. *)
type t

(** Convert a log into a transient one. *)
val from_transient_log : Transient_log.t -> t option

val to_transient_log : t -> Transient_log.t

(** Return the ID of a log. *)
val id : t -> Uuid.t

(** Return the start date of a log. *)
val start_date : t -> Datetime.t

(** Return the end date of a log. *)
val end_date : t -> Datetime.t

(** Return the duration of a log. *)
val duration : t -> Duration.t

(** Validate a log from a rensai expression. *)
val from_rensai : t Rensai.Validation.t

(** Serialize a log result. *)
val return_rensai : (Datetime.t * t) Rensai.Ast.conv

(** Serialize a log with relative date. *)
val list_to_rensai : (Datetime.t * t list) Rensai.Ast.conv

(** Lift a log into a dumpable rensai expression. *)
val to_rensai : t -> Rensai.Ast.t

val from_file_content : (string, t) Rensai.Validation.v

(** Find the relevant file for a log. *)
val find_file_by_month : cwd:Path.t -> t -> Path.t

(** Find the file of the log. *)
val find_file : cwd:Path.t -> t -> Path.t

(** Create an ordered set of logs. *)
val truncate_list : ?len:int -> t -> t list -> Uuid.Set.t

(** Retreive sector and project. *)
val sector_and_project : t -> string * string option

(** sort a list of log by date. *)
val sort : t list -> t list

(** Add complementary metadata. *)
val add_meta : key:string -> value:string -> t -> t

(** Remove complementary metadata. *)
val remove_meta : key:string -> t -> t

(** Add complementary link. *)
val add_link : key:string -> value:Url.t -> t -> t

(** Remove complementary link. *)
val remove_link : key:string -> t -> t

(** {1 Result as a call-API} *)

module Expanded : sig
  val as_list : Context.t -> t list Rensai.Ast.conv
  val as_single : Context.t -> t Rensai.Ast.conv
  val as_option : Context.t -> t option Rensai.Ast.conv
end
