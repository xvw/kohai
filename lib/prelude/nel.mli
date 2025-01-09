(** A naive implementation of a non-empty list (essentially to collect
    errors). *)

(** Type describing a non-empty list. *)
type 'a t = ( :: ) of 'a * 'a list

(** [make x xs] creates an non-empty list. *)
val make : 'a -> 'a list -> 'a t

(** [singleton x] creates an non-empty list with one element. *)
val singleton : 'a -> 'a t

(** [init len f] is [f 0; f 1; ...; f (len-1)], evaluated left to right.
    @raise Invalid_argument if [len < 1]. *)
val init : int -> (int -> 'a) -> 'a t

(** [to_list nel] convert a non-empty list into a regular list. *)
val to_list : 'a t -> 'a list

(** [rev nel] reverse [nel]. *)
val rev : 'a t -> 'a t

(** [append xs ys] concat [xs] and [ys]. *)
val append : 'a t -> 'a t -> 'a t

(** [cons x xs] constructs a non-empty list whose head is [x] and whose tail is
    [xs]. *)
val cons : 'a -> 'a t -> 'a t

(** Equality between non-empty list. *)
val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool

(** Pretty printer for non-empty list. *)
val pp
  :  ?pp_sep:(Format.formatter -> unit -> unit)
  -> (Format.formatter -> 'a -> unit)
  -> Format.formatter
  -> 'a t
  -> unit
