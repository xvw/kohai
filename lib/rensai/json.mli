(** A wrapper for dealing with the Json library, which tries to be a
    bit clever about how to represent input data and project it into
    ideal representations. *)

(** Yojson.Safe AST. *)
type yojson =
  [ `Null
  | `Bool of bool
  | `Int of int
  | `Intlit of string
  | `Float of float
  | `String of string
  | `Assoc of (string * yojson) list
  | `List of yojson list
  | `Tuple of yojson list
  | `Variant of string * yojson option
  ]

(** Ezjsonm AST. *)
type ezjsonm =
  [ `Null
  | `Bool of bool
  | `Float of float
  | `String of string
  | `A of ezjsonm list
  | `O of (string * ezjsonm) list
  ]

(** [to_yojson rensai] Transforms a Rensai expression into a Yojson
    expression. *)
val to_yojson : Ast.t -> yojson

(** [from_yojson yojson] Transforms a Yosjon expression into a Rensai
    expression. *)
val from_yojson : yojson -> Ast.t

(** [to_ezjsonm rensai] Transforms a Rensai expression into a Ezjsonm
    expression. *)
val to_ezjsonm : Ast.t -> ezjsonm

(** [from_yojson yojson] Transforms a Ezjsonm expression into a Rensai
    expression. *)
val from_ezjsonm : ezjsonm -> Ast.t
