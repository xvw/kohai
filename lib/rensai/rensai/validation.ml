type value_error =
  | Unexpected_kind of
      { expected : Kind.t
      ; given : Kind.t
      ; value : Ast.t
      }

type 'a checked = ('a, value_error) result
type 'a t = Ast.t -> 'a checked

module Infix = struct
  let ( $ ) l f x = Result.map f (l x)

  (* let ( & ) l r x = Result.bind (l x) r *)
  let ( / ) l r x = Result.fold ~ok:Result.ok ~error:(fun _ -> r x) (l x)
end

include Infix

let strim subject = subject |> String.trim |> String.lowercase_ascii

let unexpected_kind expected value =
  let given = Kind.classify value in
  Error (Unexpected_kind { expected; given; value })
;;

let replace_expected_kind new_kind = function
  | Error (Unexpected_kind { given; value; _ }) ->
    Error (Unexpected_kind { expected = new_kind; given; value })
  | value -> value
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

let as_number kind from into equal i value =
  let x = into i in
  let y = from x in
  if not (equal i y) then unexpected_kind kind value else Ok x
;;

let from_string kind from into s value =
  let s = strim s in
  match from s with
  | None -> unexpected_kind kind value
  | Some x ->
    let y = into x in
    if String.equal s y then Ok x else unexpected_kind kind value
;;

let int ?(strict = false) = function
  | Ast.Int i -> Ok i
  | Ast.String s as value when not strict ->
    from_string Kind.Int int_of_string_opt string_of_int s value
  | Ast.Int32 i as value when not strict ->
    as_number Kind.Int Int32.of_int Int32.to_int Int32.equal i value
  | Ast.Int64 i as value when not strict ->
    as_number Kind.Int Int64.of_int Int64.to_int Int64.equal i value
  | Ast.Float i as value when not strict ->
    as_number Kind.Int Float.of_int Float.to_int Float.equal i value
  | value -> unexpected_kind Kind.Int value
;;

let int32 ?(strict = false) = function
  | Ast.Int32 i -> Ok i
  | Ast.Int i when not strict -> Ok (Int32.of_int i)
  | Ast.Int64 i as value when not strict ->
    as_number Kind.Int32 Int64.of_int32 Int64.to_int32 Int64.equal i value
  | Ast.Float i as value when not strict ->
    as_number Kind.Int32 Int32.to_float Int32.of_float Float.equal i value
  | Ast.String s as value when not strict ->
    from_string Kind.Int32 Int32.of_string_opt Int32.to_string s value
  | value -> unexpected_kind Kind.Int32 value
;;

let int64 ?(strict = false) = function
  | Ast.Int64 i -> Ok i
  | Ast.Int i when not strict -> Ok (Int64.of_int i)
  | Ast.Int32 i when not strict -> Ok (Int64.of_int32 i)
  | Ast.Float i as value when not strict ->
    as_number Kind.Int64 Int64.to_float Int64.of_float Float.equal i value
  | Ast.String s as value when not strict ->
    from_string Kind.Int64 Int64.of_string_opt Int64.to_string s value
  | value -> unexpected_kind Kind.Int64 value
;;

let float ?(strict = false) = function
  | Ast.Float f -> Ok f
  | Ast.Int i as value when not strict ->
    as_number Kind.Float int_of_float float_of_int Int.equal i value
  | Ast.Int32 i as value when not strict ->
    as_number Kind.Float Int32.of_float Int32.to_float Int32.equal i value
  | Ast.Int64 i as value when not strict ->
    as_number Kind.Float Int64.of_float Int64.to_float Int64.equal i value
  | Ast.String s as value when not strict ->
    (* Canot rely on round-trip because "3" is, ie, a valid float. *)
    (match float_of_string_opt s with
     | Some x -> Ok x
     | None -> unexpected_kind Kind.Float value)
  | value -> unexpected_kind Kind.Float value
;;

let integer ?(strict = false) subject =
  subject
  |> int64 ~strict
     / (int32 ~strict $ Int64.of_int32)
     / (int ~strict $ Int64.of_int)
  |> replace_expected_kind Kind.(Int || Int32 || Int64)
;;

let number ?(strict = false) subject =
  subject
  |> float ~strict / (integer ~strict $ Int64.to_float)
  |> replace_expected_kind Kind.(Int || Int32 || Int64 || Float)
;;
