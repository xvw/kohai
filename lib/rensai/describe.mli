(** Used to describe generic data structures (which can be serialized
    or deserialized to arbitrary formats). Moving to a generic format
    is not the most efficient approach, but it is easy and (from our
    point of view) pleasant to use.

    In a way, it's a very simple and naive form of a runtime type. *)

(**/**)

(**  {@ocaml[
       open Rensai.Describe
     ]} *)

(**/**)

(** {1 Types} *)

(** The type describing the AST of a generic data structure. *)
type t = private
  | Null
  | Unit
  | Bool of bool
  | Char of char
  | Int of int
  | Int32 of int32
  | Int64 of int64
  | Float of float
  | String of string
  | Pair of t * t
  | List of t list
  | Constr of string * t
  | Record of (string * t) list

(** A shortcut for describing conversions from arbitrary values to a
    generic representation. *)
type 'a conv = 'a -> t

(** {1 Data converters}

    As the AST is a private representation (to avoid, for example,
    building heterogeneous lists), explicit converters are used to
    describe the data. *)

(** {2 Primitive types} *)

(** [null] describes a [Null] type value. *)
val null : t

(** [unit] describes a [Unit] type value. *)
val unit : t

(** [bool x] transforms [x], a Boolean, into [Bool]. *)
val bool : bool conv

(** [int x] transforms [x], an Integer, into [Int]. *)
val int : int conv

(** [int32 x] transforms [x], an Integer (32-bits), into [Int32]. *)
val int32 : int32 conv

(** [int64 x] transforms [x], an Integer (64-bits), into [Int64]. *)
val int64 : int64 conv

(** [float x] transforms [x], a Float, into [Float]. *)
val float : float conv

(** [char x] transforms [x], a Character, into [Char]. *)
val char : char conv

(** [string x] transforms [x], a String, into [String]. *)
val string : string conv

(** [list conv values] transforms a list ([values]) by applying the
    given converter ([conv]) to each element.*)
val list : 'a conv -> 'a list conv

(** {2 Product types}

    AST supports a minimal product form using the [Pair] and [Records]
    constructors. *)

(** [pair a b (x, y)] transform the product [x, y] into [Pair] using
    the converters [a] (for [x]) and [b] (for [y]). *)
val pair : 'a conv -> 'b conv -> ('a * 'b) conv

(** [pair' a b x y] is [pair a b (x, y)]. An curried version of {!val:pair}. *)
val pair' : 'a conv -> 'b conv -> 'a -> 'b conv

(** [triple a b c (x, y, z)] transform the product [x, y, z] into
    [Pair] using the converters [a] (for [x]), [b] (for [y]) and [c]
    (for [z]). Internally, a [triple] is defined as a [pair of pair]:
    [x, y, z] is [x, (y, z)]. *)
val triple : 'a conv -> 'b conv -> 'c conv -> ('a * 'b * 'c) conv

(** [triple' a b c x y z] is [triple a b c (x, y, z)]. An curried
    version of {!val:triple}. *)
val triple' : 'a conv -> 'b conv -> 'c conv -> 'a -> 'b -> 'c conv

(** [quad a b c d (w, x, y, z)] transform the product [w, x, y, z]
    into [Pair] using the converters [a] (for [w]), [b] (for [x]), [c]
    (for [y]) and [d] (for [z]). Internally, a [quad] is defined as a
    [pair of pair]: [w, x, y, z] is [w, (x, (y, z))]. *)
val quad : 'a conv -> 'b conv -> 'c conv -> 'd conv -> ('a * 'b * 'c * 'd) conv

(** [quad' a b c d w x y z] is [quad a b c d (w, x, y, z)]. An curried
    version of {!val:quad}. *)
val quad'
  :  'a conv
  -> 'b conv
  -> 'c conv
  -> 'd conv
  -> 'a
  -> 'b
  -> 'c
  -> 'd conv

(** [record assoc] describes an associative list as a record. Note
    that the function does not ensure that each field is unique. *)
val record : (string * t) list conv

(** {2 Sum types}

    It is also possible to describe generic encoders for sum types,
    using the [Constr] constructor. *)

(** [constr f x] constructs a generic representation of sums. The [f]
    function returns an encoded value and a generic representation.

    For example, let's imagine the following type:

    {@ocaml[
      type my_dummy_type =
        | Foo
        | Bar of int
        | Baz of string
    ]}

    We could imagine the encoded transformation as follows:

    {mdx@ocaml[
      # constr (function
          | Foo -> "foo", unit
          | Bar x -> "bar", int x
          | Baz s -> "baz", string s
        ) ;;
      - : my_dummy_type conv = <fun>
    ]mdx} *)
val constr : ('a -> string * t) -> 'a conv

(** [option some_conv x] a converter for ['a options]. If the option
    is [None]. The converter will return [Null]. *)
val option : 'a conv -> 'a option conv

(** [either left_conv right_conv] a converter for [('a, 'b) Either.t]. *)
val either : 'left conv -> 'right conv -> ('left, 'right) Either.t conv

(** [either left_conv right_conv] a converter for [('a, 'b) result]. *)
val result : 'ok conv -> 'err conv -> ('ok, 'err) result conv

(** {1 Combinators}

    type {!type:conv} is in fact a contravariant functor. It is
    therefore possible to express some combinators. *)

(** [use f c] is contramap. If we have a function from [a] to [b] and
    a [conv] of [b]. We also have a [conv] of [a].

    For example using a [string conv], relaying on [string_of_int],
    let's have a [int conv]:

    {mdx@ocaml[
      # use string_of_int string ;;
      - : int conv = <fun>
    ]mdx} *)
val use : ('a -> 'b) -> 'b conv -> 'a conv

(** [replace x c] is [use (fun _ -> x) c]. *)
val replace : 'b -> 'b conv -> 'a conv

(** {2 Infix}

    Even if, in practice, we prefer to keep our encoders as clear as
    possible (by making them explicit), we can take advantage of
    certain operators to compose them arbitrarily. *)

module Infix : sig
  (** [f <$> x] is [use f x]. *)
  val ( <$> ) : ('a -> 'b) -> 'b conv -> 'a conv
end

include module type of Infix (** @inline *)

(** {1 Misc} *)

(** [equal a b] return [true] if [a] equal [b].  Note that records are
    compared in a case-insensitive way. In example:

    {mdx@ocaml[
      # equal (record ["a", unit; "b", null])
              (record ["b", null; "a", unit]) ;;
      - : bool = true
    ]mdx} *)
val equal : t -> t -> bool

(** {1 Example}

    Let's imagine these different types:

    {@ocaml[
      type gender =
        | Male
        | Female
        | Other of string

      type user =
        { first_name : string
        ; last_name : string
        ; is_active : bool
        ; gender : gender
        ; crowd : user list
        }

      let user ?(crowd = []) ?(is_active = true) first_name last_name gender =
        { first_name; last_name; is_active; gender; crowd }
      ;;
    ]}

    These could, for example, be described using the following
    different encoders:

    {@ocaml[
      let conv_gender =
        constr (function
          | Male -> "male", unit
          | Female -> "female", unit
          | Other s -> "other", string s)
      ;;
    ]}

    {@ocaml[
      let rec conv_user { first_name; last_name; is_active; gender; crowd } =
        record
          [ "first_name", string first_name
          ; "last_name", string last_name
          ; "is_active", bool is_active
          ; "gender", conv_gender gender
          ; "crowd", list conv_user crowd
          ]
      ;;
    ]}

    And we can now use it to encode arbitrary users:

    {mdx@ocaml[
      # conv_user @@ user "A" "B" Male ;;
      - : t =
      Record
       [("first_name", String "A"); ("last_name", String "B");
        ("is_active", Bool true); ("gender", Constr ("male", Unit));
        ("crowd", List [])]
    ]mdx}

    {mdx@ocaml[
      # conv_user (
           user ~crowd:[user "C" "D" Male; user "E" "F" Male]
           ~is_active:false "A" "B" Female);;
      - : t =
      Record
       [("first_name", String "A"); ("last_name", String "B");
        ("is_active", Bool false); ("gender", Constr ("female", Unit));
        ("crowd",
         List
          [Record
            [("first_name", String "C"); ("last_name", String "D");
             ("is_active", Bool true); ("gender", Constr ("male", Unit));
             ("crowd", List [])];
           Record
            [("first_name", String "E"); ("last_name", String "F");
             ("is_active", Bool true); ("gender", Constr ("male", Unit));
             ("crowd", List [])]])]
    ]mdx} *)
