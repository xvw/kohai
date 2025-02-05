(** A naive library for manipulating datetime. *)

(** {1 Types} *)

(** A type describing a month. *)
type month =
  | Jan
  | Feb
  | Mar
  | Apr
  | May
  | Jun
  | Jul
  | Aug
  | Sep
  | Oct
  | Nov
  | Dec

(** A type describing a datetime. *)
type t = private
  { year : int
  ; month : month
  ; day : int
  ; hour : int
  ; min : int
  ; sec : int
  }

(** {1 Building dates} *)

(** [make ?time ~year ~month ~day ()] build a date with a lot of
    validation. *)
val make
  :  ?time:int * int * int
  -> year:int
  -> month:month
  -> day:int
  -> unit
  -> t Rensai.Validation.checked

(** {2 Constants} *)

(** [unix] return a datetime object for [1970:01:01T00:00:00]. *)
val unix : t
