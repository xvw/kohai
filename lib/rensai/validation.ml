open Prelude

type value_error =
  | Unexpected_kind of
      { expected : Kind.t
      ; given : Kind.t
      ; value : Ast.t
      }
  | Unexpected_pair of
      { error : pair_error
      ; value : Ast.t
      ; given : Kind.t
      }
  | Unexpected_list of
      { errors : (int * value_error) Nel.t
      ; value : Ast.t
      ; given : Kind.t
      }
  | Unexpected_record of
      { errors : record_error Nel.t
      ; value : Ast.t
      }
  | Unexpected_value of string

and pair_error =
  | Invalid_fst of value_error
  | Invalid_snd of value_error
  | Invalid_both of value_error * value_error

and record_error =
  | Invalid_field of
      { field : string
      ; error : value_error
      }
  | Missing_field of string

let pp_record l = Fmt.braces (Fmt.record ~sep:(Fmt.any ";@,") l)

let rec pp_value_error st = function
  | Unexpected_value message ->
    pp_record
      Fmt.[ field "message" fst Dump.string; field "error" snd Dump.string ]
      st
      ("unexpected value", message)
  | Unexpected_kind { expected; given; value } ->
    pp_record
      Fmt.
        [ field "message" (fun (x, _, _, _) -> x) Dump.string
        ; field "expected" (fun (_, x, _, _) -> x) Kind.pp
        ; field "given" (fun (_, _, x, _) -> x) Kind.pp
        ; field "value" (fun (_, _, _, x) -> x) Ast.pp
        ]
      st
      ("unexpected kind", expected, given, value)
  | Unexpected_pair { error; value; given } ->
    pp_record
      Fmt.
        [ field "message" (fun (x, _, _, _) -> x) Dump.string
        ; field "where" (fun (_, _, _, x) -> x) pp_pair_error
        ; field "given" (fun (_, x, _, _) -> x) Kind.pp
        ; field "value" (fun (_, _, x, _) -> x) Ast.pp
        ]
      st
      ("unexpected pair", given, value, error)
  | Unexpected_list { errors; value; given } ->
    pp_record
      Fmt.
        [ field "message" (fun (x, _, _, _) -> x) Dump.string
        ; field "where" (fun (_, _, _, x) -> x) (Dump.list pp_indexed_error)
        ; field "given" (fun (_, x, _, _) -> x) Kind.pp
        ; field "value" (fun (_, _, x, _) -> x) Ast.pp
        ]
      st
      ("unexpected list", given, value, Nel.to_list errors)
  | Unexpected_record { value; errors } ->
    pp_record
      Fmt.
        [ field "message" (fun (x, _, _) -> x) Dump.string
        ; field "where" (fun (_, _, x) -> x) (Dump.list pp_record_error)
        ; field "value" (fun (_, x, _) -> x) Ast.pp
        ]
      st
      ("unexpected record", value, Nel.to_list errors)

and pp_record_error st = function
  | Missing_field field ->
    pp_record
      Fmt.[ field "message" fst Dump.string; field "field" snd Dump.string ]
      st
      ("missing field", field)
  | Invalid_field { field; error } ->
    pp_record
      Fmt.
        [ field "message" (fun (x, _, _) -> x) Dump.string
        ; field "field" (fun (_, x, _) -> x) Dump.string
        ; field "where" (fun (_, _, x) -> x) pp_value_error
        ]
      st
      ("invalid field", field, error)

and pp_indexed_error st (index, error) =
  Fmt.pf st "%02d:@,@[<h>%a@]" index pp_value_error error

and pp_pair_error st = function
  | Invalid_fst err ->
    pp_record
      Fmt.[ field "on" fst Dump.string; field "error" snd pp_value_error ]
      st
      ("first", err)
  | Invalid_snd err ->
    pp_record
      Fmt.[ field "on" fst Dump.string; field "error" snd pp_value_error ]
      st
      ("second", err)
  | Invalid_both (errf, errs) ->
    pp_record
      Fmt.
        [ field "on" (fun (x, _, _) -> x) Dump.string
        ; field "first_error" (fun (_, x, _) -> x) pp_value_error
        ; field "second_error" (fun (_, _, x) -> x) pp_value_error
        ]
      st
      ("both", errf, errs)
;;

let pp_checked pp = Fmt.result ~ok:pp ~error:pp_value_error

type 'a checked = ('a, value_error) result
type 'a checked_record = ('a, record_error Nel.t) result
type ('a, 'b) v = 'a -> 'b checked
type 'a t = (Ast.t, 'a) v
type 'a record_validator = (string * Ast.t) list -> 'a checked_record

module Infix = struct
  let ( <$> ) = Result.map
  let ( $ ) l f x = Result.map f (l x)
  let ( & ) l r x = Result.bind (l x) r
  let ( / ) l r x = Result.fold ~ok:Result.ok ~error:(fun _ -> r x) (l x)
  let ( % ) f g x = f (g x)
end

module Syntax = struct
  let ( let+ ) x f = Result.map f x
  let ( let* ) = Result.bind

  let ( and+ ) a b =
    match a, b with
    | Ok x, Ok y -> Ok (x, y)
    | Error err, _ | _, Error err -> Error err
  ;;

  let ( and* ) = ( and+ )
end

include Infix
include Syntax

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

let unexpected_list value errors =
  let given = Kind.classify value in
  Unexpected_list { value; given; errors = Nel.rev errors }
;;

let unexpected_pair value error =
  let given = Kind.classify value in
  Error (Unexpected_pair { error; given; value })
;;

let invalid_field field error = Nel.singleton @@ Invalid_field { field; error }
let missing_field field _discard_error = Nel.singleton @@ Missing_field field
let unexpected_record value errors = Unexpected_record { value; errors }

let unexpected_value message subject =
  Error (Unexpected_value (message subject))
;;

let force_error_message message = function
  | Ok x -> Ok x
  | Error _ -> Error (Unexpected_value message)
;;

let const x _discarded = Ok x
let default_pp x = Fmt.any "?value" x

let where
      ?(pp = default_pp)
      ?(message = Fmt.str "unsatisfied predicate on %a")
      predicate
      subject
  =
  if predicate subject
  then Ok subject
  else unexpected_value (message pp) subject
;;

let unless
      ?(pp = default_pp)
      ?(message = Fmt.str "satisfied predicate on %a")
      predicate
      subject
  =
  if not (predicate subject)
  then Ok subject
  else unexpected_value (message pp) subject
;;

let refute
      ?(pp = default_pp)
      ?(message = Fmt.str "refuted validator on %a")
      validator
      x
  =
  match validator x with
  | Error _ -> Ok x
  | Ok _ -> unexpected_value (message pp) x
;;

let equal ?(pp = default_pp) ?(eq = ( = )) a =
  let message pp = Fmt.str "`a` (%a) is not equal to `b` (%a)" pp a pp in
  where ~pp ~message (eq a)
;;

let not_equal ?(pp = default_pp) ?(eq = ( = )) a =
  let message pp = Fmt.str "`a` (%a) is equal to `b` (%a)" pp a pp in
  where ~pp ~message (fun b -> not (eq a b))
;;

let greater ?(pp = default_pp) ?(compare = Stdlib.compare) ~than =
  let message pp = Fmt.str "`a` (%a) is not greater than `b` (%a)" pp than pp in
  where ~pp ~message (fun b -> compare b than > 0)
;;

let greater_or_equal ?(pp = default_pp) ?(compare = Stdlib.compare) ~than =
  let message pp =
    Fmt.str "`a` (%a) is not greater or equal to `b` (%a)" pp than pp
  in
  where ~pp ~message (fun b -> compare b than >= 0)
;;

let less ?(pp = default_pp) ?(compare = Stdlib.compare) ~than =
  let message pp = Fmt.str "`a` (%a) is not less than `b` (%a)" pp than pp in
  where ~pp ~message (fun b -> compare b than < 0)
;;

let less_or_equal ?(pp = default_pp) ?(compare = Stdlib.compare) ~than =
  let message pp =
    Fmt.str "`a` (%a) is not less or equal to `b` (%a)" pp than pp
  in
  where ~pp ~message (fun b -> compare b than <= 0)
;;

let in_range ?(pp = default_pp) ?(compare = Stdlib.compare) ~min ~max =
  let message pp x =
    Fmt.str
      "`a` (%a) is not included into [`min` (%a); `max` (%a)]"
      pp
      x
      pp
      min
      pp
      max
  in
  where ~pp ~message (fun x -> compare x min >= 0 && compare x max <= 0)
;;

let outside_range ?(pp = default_pp) ?(compare = Stdlib.compare) ~min ~max =
  let message pp x =
    Fmt.str
      "`a` (%a) is included into [`min` (%a); `max` (%a)]"
      pp
      x
      pp
      min
      pp
      max
  in
  where ~pp ~message (fun x -> compare x min < 0 || compare x max > 0)
;;

let one_of ?(pp = default_pp) ?(eq = ( = )) cases value =
  match Stdlib.List.find_opt (eq value) cases with
  | Some _ -> Ok value
  | None ->
    let message pp value =
      Fmt.str "%a is not in the list %a" pp value (Fmt.Dump.list pp) cases
    in
    unexpected_value (message pp) value
;;

module From_dumpable (D : Sigs.DUMPABLE) :
  Sigs.SIMPLE_VALIDATOR with type t := D.t and type error := value_error =
struct
  include D

  let where = where ~pp
  let unless = unless ~pp
  let refute = refute ~pp
end

module From_equatable (E : Sigs.EQUATABLE) :
  Sigs.EQUATABLE_VALIDATOR with type t := E.t and type error := value_error =
struct
  let eq = E.equal
  let pp = E.pp
  let equal = equal ~pp ~eq
  let not_equal = not_equal ~pp ~eq
  let one_of = one_of ~pp ~eq
end

module From_comparable (C : Sigs.COMPARABLE) :
  Sigs.COMPARABLE_VALIDATOR with type t := C.t and type error := value_error =
struct
  include C

  let greater = greater ~pp ~compare
  let greater_or_equal = greater_or_equal ~pp ~compare
  let less = less ~pp ~compare
  let less_or_equal = less_or_equal ~pp ~compare
  let in_range = in_range ~pp ~compare
  let outside_range = outside_range ~pp ~compare
end

module From_number (N : Sigs.NUMBER) :
  Sigs.NUMBER_VALIDATOR with type t := N.t and type error := value_error =
struct
  include N

  let eq a b = Int.equal 0 (compare a b)
  let is_null = equal ~pp ~eq zero
  let is_not_null = not_equal ~pp ~eq zero
  let is_positive = greater_or_equal ~pp ~compare ~than:zero
  let is_negative = less ~pp ~compare ~than:zero
end

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

let string ?(strict = false) = function
  | Ast.String s -> Ok s
  | Ast.Bool b when not strict -> Ok (string_of_bool b)
  | Ast.Char c when not strict -> Ok (String.make 1 c)
  | Ast.Int i when not strict -> Ok (string_of_int i)
  | Ast.Int32 i when not strict -> Ok (Int32.to_string i)
  | Ast.Int64 i when not strict -> Ok (Int64.to_string i)
  | Ast.Float i when not strict -> Ok (Float.to_string i)
  | value -> unexpected_kind Kind.String value
;;

let temp_pair fst snd = function
  | (Ast.Pair (a, b) | Ast.List [ a; b ]) as value ->
    (match fst a, snd b with
     | Ok x, Ok y -> Ok (x, y)
     | Error x, Ok _ -> unexpected_pair value (Invalid_fst x)
     | Ok _, Error x -> unexpected_pair value (Invalid_snd x)
     | Error x, Error y -> unexpected_pair value (Invalid_both (x, y)))
  | value -> unexpected_kind Kind.(Pair (Any, Any)) value
;;

let list = function
  | Ast.List list -> Ok list
  | value -> unexpected_kind Kind.(List Any) value
;;

let list_of v = function
  | Ast.List list as value ->
    List.rev
    <$> snd
        @@ List.fold_left
             (fun (i, acc) value ->
                let acc =
                  match acc, v value with
                  | Ok xs, Ok x -> Ok (x :: xs)
                  | Error xs, Error x -> Error (Nel.cons (i, x) xs)
                  | Error e, _ -> Error e
                  | _, Error e -> Error (Nel.singleton (i, e))
                in
                i + 1, acc)
             (0, Ok [])
             list
    |> Result.map_error (unexpected_list value)
  | value -> unexpected_kind Kind.(List Any) value
;;

let find_assoc ?(normalize = true) key assoc =
  List.find_map
    (fun (k, v) ->
       let eq =
         if normalize
         then String.equal (strim key) (strim k)
         else String.equal key k
       in
       if eq then Some v else None)
    assoc
;;

let record ?(strict = false) validator expr =
  let rec aux = function
    | Ast.Record fields as value ->
      fields
      |> Ast.record_to_assoc
      |> validator
      |> Result.map_error (unexpected_record value)
    | Pair (String k, expr) when not strict -> aux (Ast.record [ k, expr ])
    | value ->
      (match strict, list_of (temp_pair string (fun x -> Ok x)) value with
       | false, Ok xs -> aux (Ast.record xs)
       | _, _ -> unexpected_kind Kind.Record value)
  in
  aux expr
;;

module Record = struct
  let optional fields key validator =
    match find_assoc ~normalize:false key fields with
    | None | Some Ast.Null -> Ok None
    | Some field_value ->
      field_value
      |> (validator $ Option.some)
      |> Result.map_error (invalid_field key)
  ;;

  let required fields key validator =
    let* as_optional = optional fields key validator in
    match as_optional with
    | Some x -> Ok x
    | None ->
      (* Handle the case where the validator deal with an optional
         one (without relaying on [optional]) *)
      Ast.null () |> validator |> Result.map_error (missing_field key)
  ;;

  let optional_or ~default fields key validator =
    let+ x = validator |> optional fields key in
    Option.value ~default x
  ;;

  module Syntax = struct
    let ( let+ ) v f = Result.map f v
    let ( let* ) v f = Result.bind v f

    let ( and+ ) a b =
      match a, b with
      | Ok x, Ok y -> Ok (x, y)
      | Error a, Error b -> Error (Nel.append a b)
      | Error a, _ | _, Error a -> Error a
    ;;

    let ( and* ) = ( and+ )
  end

  include Syntax
end

let pair fst snd = function
  | Ast.Record _ as value ->
    (record (fun fields ->
       let open Record in
       let+ fst = required fields "fst" fst
       and+ snd = required fields "snd" snd in
       fst, snd)
     / record (fun fields ->
       let open Record in
       let+ fst = required fields "first" fst
       and+ snd = required fields "second" snd in
       fst, snd))
      value
  | value -> temp_pair fst snd value
;;

let triple fst snd trd x =
  let rec aux = function
    (* Be more lax on tuple treatement. *)
    | Ast.List [ a; b; c ] -> aux Ast.(lpair a (lpair b c))
    | value -> (pair fst (pair snd trd) $ fun (x, (y, z)) -> x, y, z) value
  in
  aux x
;;

let quad fst snd trd frd x =
  let rec aux = function
    (* Be more lax on tuple treatement. *)
    | Ast.List [ a; b; c; d ] -> aux Ast.(lpair a (lpair b (lpair c d)))
    | value ->
      (temp_pair fst (pair snd (pair trd frd))
       $ fun (w, (x, (y, z))) -> w, x, y, z)
        value
  in
  aux x
;;

let sum ?(strict = false) ctors expr =
  let kind =
    ctors
    |> List.map (fun (ctor, _) -> Kind.Constr (strim ctor, Any))
    |> Kind.from_list
  in
  let rec aux = function
    | Ast.Constr (ctor, expr) as value ->
      ctors
      |> find_assoc ctor
      |> Option.fold ~none:(unexpected_kind kind value) ~some:(fun validator ->
        validator expr)
    | (Pair (String ctor, expr) | List [ String ctor; expr ]) when not strict ->
      aux (Ast.constr (fun _ -> ctor, expr) ())
    | Record _ as value when not strict ->
      let* ctor, value =
        record
          (fun fields ->
             let open Record in
             let+ ctor = required fields "ctor" string
             and+ value =
               optional_or ~default:(Ast.null ()) fields "value" Result.ok
             in
             ctor, value)
          value
      in
      aux @@ Ast.constr (fun _ -> ctor, value) ()
    | value -> unexpected_kind kind value
  in
  aux expr
;;

let option some = function
  | Ast.Null -> Ok None
  | value -> Option.some <$> some value
;;

let either ?strict left right =
  sum ?strict [ "left", left $ Either.left; "right", right $ Either.right ]
;;

let result ?strict ok error =
  sum ?strict [ "ok", ok $ Result.ok; "error", error $ Result.error ]
;;

module For_number (N : sig
    include Sigs.EQUATABLE
    include Sigs.NUMBER with type t := t
  end) =
struct
  include From_dumpable (N)
  include From_equatable (N)
  include From_comparable (N)
  include From_number (N)
end

module Int = For_number (struct
    include Int

    let pp = Fmt.int
  end)

module Int32 = For_number (struct
    include Int32

    let pp = Fmt.int32
  end)

module Int64 = For_number (struct
    include Int64

    let pp = Fmt.int64
  end)

module Float = For_number (struct
    include Float

    let pp = Fmt.float
  end)

module Char = struct
  module C = struct
    include Char

    let pp = Fmt.char
  end

  include From_dumpable (C)
  include From_equatable (C)
  include From_comparable (C)

  let is_digit =
    where
      ~message:(fun pp c -> Fmt.str "`%a` is not a '0' .. '9'" pp c)
      (function
        | '0' .. '9' -> true
        | _ -> false)
  ;;

  let as_digit = is_digit $ fun x -> Char.(code x - code '0')

  let is_hex_digit =
    where
      ~message:(fun pp c ->
        Fmt.str "`%a` is not a '0' .. '9' or 'a' .. 'f'" pp c)
      (function
        | '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' -> true
        | _ -> false)
  ;;

  let as_hex_digit =
    is_hex_digit
    $ function
    | '0' .. '9' as x -> Char.(code x - code '0')
    | 'a' .. 'f' as x -> Char.(code x - code 'a') + 10
    | 'A' .. 'F' as x -> Char.(code x - code 'A') + 10
    | _ -> 0 (* unreachable *)
  ;;

  let is_alpha =
    where
      ~message:(fun pp c -> Fmt.str "`%a` is not a letter" pp c)
      (function
        | 'a' .. 'z' | 'A' .. 'Z' -> true
        | _ -> false)
  ;;

  let is_alphanumeric =
    where
      ~message:(fun pp c -> Fmt.str "`%a` is not a letter" pp c)
      (function
        | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' -> true
        | _ -> false)
  ;;

  let is_lowercase =
    where
      ~message:(fun pp c -> Fmt.str "`%a` is not a lowercased letter" pp c)
      (function
        | 'a' .. 'z' -> true
        | _ -> false)
  ;;

  let is_uppercase =
    where
      ~message:(fun pp c -> Fmt.str "`%a` is not a uppercased letter" pp c)
      (function
        | 'A' .. 'Z' -> true
        | _ -> false)
  ;;

  let is_whitespace =
    where
      ~message:(fun pp c -> Fmt.str "`%a` is not a whitespace" pp c)
      (function
        | ' ' | '\t' | '\n' | '\011' | '\012' | '\r' -> true
        | _ -> false)
  ;;

  let is_newline =
    where
      ~message:(fun pp c -> Fmt.str "`%a` is not a newline" pp c)
      (function
        | '\n' -> true
        | _ -> false)
  ;;
end

module Bool = struct
  module B = struct
    include Bool

    let pp = Fmt.bool
  end

  include From_dumpable (B)
  include From_equatable (B)
  include From_comparable (B)

  let is_true = equal true
  let is_false = equal false
  let negate x = Ok (not x)
end

module String = struct
  module S = struct
    include String

    let pp = Fmt.Dump.string
  end

  include From_dumpable (S)
  include From_equatable (S)
  include From_comparable (S)

  let trim x = Ok (String.trim x)
  let downcase x = Ok (String.lowercase_ascii x)
  let upcase x = Ok (String.uppercase_ascii x)
  let capitalize x = Ok (String.capitalize_ascii x)
  let uncapitalize x = Ok (String.uncapitalize_ascii x)

  let is_empty x =
    ((force_error_message @@ Fmt.str "`%s` is not empty" x) % equal "") x
  ;;

  let is_not_empty x =
    ((force_error_message @@ Fmt.str "`%s` is empty" x) % not_equal "") x
  ;;

  let is_blank x =
    ((force_error_message @@ Fmt.str "`%s` is not blank" x)
     % (trim & is_empty & const x))
      x
  ;;

  let is_not_blank x =
    ((force_error_message @@ Fmt.str "`%s` is blank" x)
     % (trim & is_not_empty & const x))
      x
  ;;

  let start_with x =
    where
      ~message:(fun pp s -> Fmt.str "`%a` is not starting by `%a`" pp s pp x)
      (String.starts_with ~prefix:x)
  ;;

  let ends_with x =
    where
      ~message:(fun pp s -> Fmt.str "`%a` is not ending by `%a`" pp s pp x)
      (String.ends_with ~suffix:x)
  ;;

  let is_slug ?(accept_capital = false) ?(unknown = '-') ?(separator = '-') =
    where
      ~message:(Fmt.str "`%a` looks not like a slug")
      (String.for_all (function
         | '0' .. '9' | 'a' .. 'z' -> true
         | 'A' .. 'Z' when accept_capital -> true
         | c -> Stdlib.Char.(equal separator c || equal unknown c)))
  ;;
end
