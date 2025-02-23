(** Transient logs are logs that are stored temporarily before being
    reviewed and promoted to regular logs. (Allowing you to record
    only the minimum and then assign additional operations to them). *)

(** {1 Types} *)

(** Describing a transient log. *)
type t

(** Describing a result after inserting a new log. *)
type result

(** A type describing the operations that can be applied to transient
    logs. *)
type operation = private
  | Record of
      { date_query : Datetime.Query.t option
      ; project : string option
      ; sector : string
      ; label : string
      }
  | Stop_recording of
      { index : int
      ; duration : int option
      }
  | Rewrite of
      { index : int
      ; date_query : Datetime.Query.t option
      ; project : string option
      ; sector : string
      ; label : string
      }
  | Delete of { index : int }
  | Add_meta of
      { index : int
      ; key : string
      ; value : string
      }
  | Remove_meta of
      { index : int
      ; key : string
      }
  | Add_link of
      { index : int
      ; key : string
      ; value : Url.t
      }
  | Remove_link of
      { index : int
      ; key : string
      }

(** {1 API} *)

(** Build a transient log. *)
val make
  :  start_date:Datetime.t
  -> project:string option
  -> sector:string
  -> label:string
  -> t

(** [has_index n log] Return [true] if the given log has the given index. *)
val has_index : int -> t -> bool

(** Converter of transient log to Rensai. *)
val from_rensai : t Rensai.Validation.t

(** Serialize a transient log. *)
val to_rensai : t Rensai.Ast.conv

(** Serialize a transient log with relative date. *)
val list_to_rensai : (Datetime.t * t list) Rensai.Ast.conv

(** Read a list of transient logs from a file content. *)
val from_file_content : string -> t list

(** Recompute the given log with a potential duration (or a
    datetime). *)
val finalize_duration : Datetime.t -> t -> int option -> t

(** compute a result from an inserted log and the full list of
    logs. *)
val to_result : ?inserted:t -> t list -> result

(** Properly sort and index a list of logs. *)
val sort : t list -> t list

(** Serialize an insertion result. *)
val result_to_rensai : (Datetime.t * result) Rensai.Ast.conv

(** Read an operation from a Rensai representation. *)
val operation_from_rensai : operation Rensai.Validation.t

(** Render a list of transient logs into a string to be stored in a file. *)
val dump : result -> string

(** Add complementary metadata. *)
val add_meta : key:string -> value:string -> t -> t

(** Remove complementary metadata. *)
val remove_meta : key:string -> t -> t

(** Add complementary link. *)
val add_link : key:string -> value:Url.t -> t -> t

(** Remove complementary link. *)
val remove_link : key:string -> t -> t
