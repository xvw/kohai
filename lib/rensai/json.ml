type yojson =
  [ `Null
  | `Bool of bool
  | `Int of int
  | `Intlit of string
  | `Float of float
  | `String of string
  | `Assoc of (string * yojson) list
  | `List of yojson list
  ]

type ezjsonm =
  [ `Null
  | `Bool of bool
  | `Float of float
  | `String of string
  | `A of ezjsonm list
  | `O of (string * ezjsonm) list
  ]

let rec to_ezjsonm = function
  | Ast.Null | Ast.Unit -> `Null
  | Ast.Bool b -> `Bool b
  | Ast.Int x -> `Float (float_of_int x)
  | Ast.Float x -> `Float x
  | Ast.Int32 x -> `String (Int32.to_string x ^ "l")
  | Ast.Int64 x -> `String (Int64.to_string x ^ "L")
  | Ast.Char x -> `String (String.make 1 x)
  | Ast.String x -> `String x
  | Ast.Pair (a, b) -> `O [ "fst", to_ezjsonm a; "snd", to_ezjsonm b ]
  | Ast.List xs -> `A (List.map to_ezjsonm xs)
  | Ast.Constr (k, v) -> `O [ "ctor", `String k; "value", to_ezjsonm v ]
  | Ast.Record record ->
    let record = Ast.record_to_assoc record in
    `O (List.map (fun (k, v) -> k, to_ezjsonm v) record)
;;

let rec to_yojson = function
  | Ast.Null | Ast.Unit -> `Null
  | Ast.Bool b -> `Bool b
  | Ast.Int x -> `Int x
  | Ast.Float x -> `Float x
  | Ast.Int32 x -> `Intlit (Int32.to_string x)
  | Ast.Int64 x -> `Intlit (Int64.to_string x)
  | Ast.Char x -> `String (String.make 1 x)
  | Ast.String x -> `String x
  | Ast.Pair (a, b) -> `Assoc [ "fst", to_yojson a; "snd", to_yojson b ]
  | Ast.Constr (k, Ast.Null) | Ast.Constr (k, Ast.Unit) ->
    `Assoc [ "ctor", `String k ]
  | Ast.Constr (k, v) -> `Assoc [ "ctor", `String k; "value", to_yojson v ]
  | Ast.List xs -> `List (List.map to_yojson xs)
  | Ast.Record record ->
    let record = Ast.record_to_assoc record in
    `Assoc (List.map (fun (k, v) -> k, to_yojson v) record)
;;

let as_int64 x = Scanf.sscanf_opt x "%LdL%!" Ast.int64
let as_int32 x = Scanf.sscanf_opt x "%ldl%!" Ast.int32
let as_int x = Scanf.sscanf_opt x "%dL%!" Ast.int

let ( <|> ) a b =
  match a with
  | Some x -> Some x
  | None -> b
;;

let infer_string s =
  match as_int64 s <|> as_int32 s <|> as_int s with
  | None -> Ast.string s
  | Some x -> x
;;

let infer_float f =
  match Float.classify_float (fst (Float.modf f)) with
  | Float.FP_zero -> Ast.int (int_of_float f)
  | _ -> Ast.float f
;;

let infer_record fix = function
  | [ ("fst", a); ("snd", b) ] | [ ("first", a); ("second", b) ] ->
    Ast.pair fix fix (a, b)
  | [ ("ctor", `String ctor); ("value", value) ]
  | [ ("constr", `String ctor); ("value", value) ] ->
    Ast.constr (fun () -> ctor, fix value) ()
  | [ ("ctor", `String ctor) ] | [ ("constr", `String ctor) ] ->
    Ast.constr (fun () -> ctor, Ast.unit ()) ()
  | fields -> Ast.record (List.map (fun (k, v) -> k, fix v) fields)
;;

let rec from_yojson = function
  | `Null -> Ast.null ()
  | `Bool b -> Ast.bool b
  | `Int x -> Ast.int x
  | `Intlit x -> infer_string x
  | `Float f -> infer_float f
  | `String s -> infer_string s
  | `List xs -> Ast.list from_yojson xs
  | `Assoc record -> infer_record from_yojson record
;;

let rec from_ezjsonm = function
  | `Null -> Ast.null ()
  | `Bool b -> Ast.bool b
  | `Float f -> infer_float f
  | `String s -> infer_string s
  | `A xs -> Ast.list from_ezjsonm xs
  | `O record -> infer_record from_ezjsonm record
;;
