type value_error =
  | Unexpected_kind of
      { expected : Kind.t
      ; given : Kind.t
      ; value : Ast.t
      }

type 'a checked = ('a, value_error) result
type 'a t = Ast.t -> 'a checked

let strim subject = subject |> String.trim |> String.lowercase_ascii

let unexpected_kind expected value =
  let given = Kind.classify value in
  Error (Unexpected_kind { expected; given; value })
;;

let null = function
  | Ast.Null -> Ok ()
  | value -> unexpected_kind Kind.Null value
;;

let unit = function
  | Ast.Unit -> Ok ()
  | value -> unexpected_kind Kind.Unit value
;;

let unitish = function
  | Ast.Unit | Ast.Null -> Ok ()
  | value -> unexpected_kind Kind.(Unit || Null) value
;;

let bool ?(strict = false) = function
  | Ast.Bool x -> Ok x
  | Ast.String s as value when not strict ->
    (match strim s with
     | "true" -> Ok true
     | "false" -> Ok false
     | _ -> unexpected_kind Kind.Bool value)
  | value -> unexpected_kind Kind.Bool value
;;

let char ?(strict = false) = function
  | Ast.Char c -> Ok c
  | Ast.String s when (not strict) && Int.equal 1 (String.length s) -> Ok s.[0]
  | value ->
    (* Maybe handle integers as potential chars. *)
    unexpected_kind Kind.Char value
;;

let int ?(strict = false) = function
  | Ast.Int i -> Ok i
  | Ast.String s as value when not strict ->
    (match int_of_string_opt s with
     | None -> unexpected_kind Kind.Int value
     | Some i -> Ok i)
  | Ast.Int32 i as value when not strict ->
    let x = Int32.to_int i in
    let y = Int32.of_int x in
    if not (Int32.equal i y) then unexpected_kind Kind.Int value else Ok x
  | Ast.Int64 i as value when not strict ->
    let x = Int64.to_int i in
    let y = Int64.of_int x in
    if not (Int64.equal i y) then unexpected_kind Kind.Int value else Ok x
  | Ast.Float i as value when not strict ->
    let x = Float.to_int i in
    let y = Float.of_int x in
    if not (Float.equal i y) then unexpected_kind Kind.Int value else Ok x
  | value -> unexpected_kind Kind.Int value
;;
