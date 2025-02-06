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

(** An amount for operation on dates. *)
type op

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

(** {1 Date information} *)

(** Returns the day of week of a given datetime. (the Algorithm is
    pretty naïve) *)
val day_of_week : t -> day_of_week

(** Return the number of days in the given month. *)
val days_in_month : t -> int

(** {1 Date manipulation} *)

(** compute a new date at the begining of the day. *)
val begin_of_day : t -> t

(** compute a new date at the end of the day. *)
val end_of_day : t -> t

(** compute a new date at the begining of the week. *)
val begin_of_week : t -> t

(** compute a new date at the end of the week. *)
val end_of_week : t -> t

(** compute a new date at the begining of the month. *)
val begin_of_month : t -> t

(** compute a new date at the end of the month. *)
val end_of_month : t -> t

(** compute a new date at the begining of the year. *)
val begin_of_year : t -> t

(** compute a new date at the end of the year. *)
val end_of_year : t -> t

(** compute a date at the begining of the next year. *)
val succ_year : t -> t

(** compute a date at the begining of the previous year. *)
val pred_year : t -> t

(** compute a date at the begining of the next month. *)
val succ_month : t -> t

(** compute a date at the begining of the previous month. *)
val pred_month : t -> t

(** compute a date at the begining of the next week. *)
val succ_week : t -> t

(** compute a date at the begining of the previous week. *)
val pred_week : t -> t

(** compute a date at the begining of the next day. *)
val succ_day : t -> t

(** compute a date at the begining of the previous day. *)
val pred_day : t -> t

(** compute a date at the begining of the next hour. *)
val succ_hour : t -> t

(** compute a date at the begining of the previous hour. *)
val pred_hour : t -> t

(** compute a date at the begining of the next min. *)
val succ_min : t -> t

(** compute a date at the begining of the previous min. *)
val pred_min : t -> t

(** compute a date at the begining of the next sec. *)
val succ_sec : t -> t

(** compute a date at the begining of the previous sec. *)
val pred_sec : t -> t

(** {2 Basic arithmetic}

    Arithmetic operations are a bit greedy, as they loop over the
    various succ and pred operations. But the aim is essentially to
    perform small operations. *)

(** [add op dt] perform the operation on [dt]. i.e: [add (day 3) dt]. *)
val add : op -> t -> t

val min : int -> op
val sec : int -> op
val hour : int -> op
val day : int -> op
val week : int -> op
val month : int -> op
val year : int -> op

module Infix : sig
  val ( + ) : t -> op -> t
  val ( - ) : t -> op -> t
end

include module type of Infix

(** {1 Misc} *)

(** [compare x y] comparison between two datetime. *)
val compare : t -> t -> int

(** [equal x y] equality between two datetime. *)
val equal : t -> t -> bool

(** Simple Pretty printer for datetime (according to rfc3339). *)
val pp_rfc3339 : ?tz:string -> unit -> Format.formatter -> t -> unit

(** Simple Pretty printer for datetime (according to rfc822). *)
val pp_rfc822 : ?tz:string -> unit -> Format.formatter -> t -> unit

(** Comparison between day of week. *)
val compare_day_of_week : day_of_week -> day_of_week -> int

(** Equality between day of week. *)
val equal_day_of_week : day_of_week -> day_of_week -> bool

(** Pretty printers for day of week. *)
val pp_day_of_week : Format.formatter -> day_of_week -> unit
