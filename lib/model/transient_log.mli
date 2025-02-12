(** Transient logs are logs that are stored temporarily before being
    reviewed and promoted to regular logs. (Allowing you to record
    only the minimum and then assign additional operations to them). *)

(** {1 Types} *)

(** Describing a transient log. *)
type t

(** Describing a result after inserting a new log. *)
type after_insertion

(** A type describing the operations that can be applied to transient
    logs. *)
type operation = private
  | Record of
      { start_date : Datetime.t option
      ; project : string option
      ; sector : string
      ; label : string
      }
  | Stop_recording of
      { index : int
      ; duration : int option
      }

(** {1 API} *)

(** Build a transient log. *)
val make
  :  start_date:Datetime.t
  -> project:string option
  -> sector:string
  -> label:string
  -> t

(** Converter of transient log to Rensai. *)
val from_rensai : t Rensai.Validation.t

(** Serialize a transient log. *)
val to_rensai : t Rensai.Ast.conv

(** Serialize an insertion result. *)
val after_insertion_to_rensai : after_insertion Rensai.Ast.conv

(** Read an operation from a Rensai representation. *)
val operation_from_rensai : operation Rensai.Validation.t
