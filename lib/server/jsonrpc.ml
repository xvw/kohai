type service =
  | Handler :
      'a Rensai.Validation.t
      * ('b -> Rensai.Ast.t)
      * (?id:int -> (module Eff.HANDLER) -> 'a -> 'b)
      -> service

let validate_request_body =
  let open Rensai.Validation in
  record (fun e ->
    let open Record in
    let+ () = ensure e "jsonrpc" (string & String.equal "2.0")
    and+ meth = required e "method" (string & String.is_not_blank)
    and+ id = optional e "id" int
    and+ params = optional_or ~default:(Rensai.Ast.null ()) e "params" ast in
    meth, id, params)
;;

let service ~meth ~with_params ~finalizer callback =
  meth, Handler (with_params, finalizer, callback)
;;

let from_response (module H : Eff.HANDLER) body =
  try
    body
    |> Yojson.Safe.from_string
    |> Rensai.Json.from_yojson
    |> validate_request_body
    |> Eff.from_result
         (module H)
         (fun error -> Error.invalid_request ~body ~error ())
  with
  | H.Jsonrpc_exn err -> Eff.raise (module H) err
  | _ -> Eff.raise (module H) (Error.parse_error ~body ())
;;

let succeed ?id value =
  let open Rensai.Ast in
  record [ "jsonrpc", string "2.0"; "id", option int id; "result", value ]
;;

let run ~services body (module H : Eff.HANDLER) =
  let services = services body in
  let meth, id, params = from_response (module H) body in
  match List.assoc_opt meth services with
  | None -> Eff.raise (module H) (Error.method_not_found ~body ?id ~meth ())
  | Some (Handler (validator, finalizer, controller)) ->
    let params =
      params
      |> validator
      |> Eff.from_result
           (module H)
           (fun error -> Error.invalid_params ~body ?id ~error ())
    in
    let result = params |> controller ?id (module H) |> finalizer in
    succeed ?id result
;;
