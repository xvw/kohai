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

(** Type describing the day of the week. *)
type day_of_week =
  | Mon
  | Tue
  | Wed
  | Thu
  | Fri
  | Sat
  | Sun

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

(** {1 Date manipulation} *)

(** Returns the day of week of a given datetime. (the Algorithm is
    pretty naïve) *)
val day_of_week : t -> day_of_week

(** {1 Misc} *)

(** [compare x y] comparison between two datetime. *)
val compare : t -> t -> int

(** [equal x y] equality between two datetime. *)
val equal : t -> t -> bool

(** Comparison between day of week. *)
val compare_day_of_week : day_of_week -> day_of_week -> int

(** Equality between day of week. *)
val equal_day_of_week : day_of_week -> day_of_week -> bool

(** Pretty printers for day of week. *)
val pp_day_of_week : Format.formatter -> day_of_week -> unit
