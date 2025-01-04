type t =
  | Null
  | Unit
  | Bool
  | Char
  | Int
  | Int32
  | Int64
  | Float
  | String
  | Pair
  | List
  | Constr
  | Record

let classify = function
  | Ast.Null -> Null
  | Ast.Unit -> Unit
  | Ast.Bool _ -> Bool
  | Ast.Char _ -> Char
  | Ast.Int _ -> Int
  | Ast.Int32 _ -> Int32
  | Ast.Int64 _ -> Int64
  | Ast.Float _ -> Float
  | Ast.String _ -> String
  | Ast.Pair (_, _) -> Pair
  | Ast.List _ -> List
  | Ast.Constr (_, _) -> Constr
  | Ast.Record _ -> Record
;;
