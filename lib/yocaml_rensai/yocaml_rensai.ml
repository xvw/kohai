module Data_provider = struct
  type t = Rensai.Ast.t

  let rec normalize = function
    | Rensai.Ast.Null -> Yocaml.Data.null
    | Unit -> Yocaml.Data.null
    | Bool b -> Yocaml.Data.bool b
    | Char c -> Yocaml.Data.string @@ String.make 1 c
    | Int i -> Yocaml.Data.int i
    | Int32 i -> Yocaml.Data.float (Int32.to_float i)
    | Int64 i -> Yocaml.Data.float (Int64.to_float i)
    | Float f -> Yocaml.Data.float f
    | String s -> Yocaml.Data.string s
    | Pair (a, b) -> Yocaml.Data.pair normalize normalize (a, b)
    | List xs -> Yocaml.Data.list_of normalize xs
    | Constr (kname, value) ->
      Yocaml.Data.sum (fun () -> kname, normalize value) ()
    | Record fields ->
      fields
      |> Rensai.Ast.record_to_assoc
      |> List.map (fun (k, v) -> k, normalize v)
      |> Yocaml.Data.record
  ;;

  let from_string given =
    given
    |> Rensai.Lang.from_string
    |> Option.to_result
         ~none:
           (Yocaml.Required.Parsing_error
              { given; message = "Rensai: unable to parse rensai expression" })
  ;;
end

include Yocaml.Make.Data_reader (Data_provider)
