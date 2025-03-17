type service =
  | Handler :
      'a Rensai.Validation.t
      * (Kohai_model.Context.t -> 'b Rensai.Ast.conv)
      * ((module Eff.HANDLER) -> Kohai_model.Context.t -> 'a -> 'b)
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

let from_response body =
  try
    body
    |> Yojson.Safe.from_string
    |> Rensai.Json.from_yojson
    |> validate_request_body
    |> Result.map_error (fun error -> Error.invalid_request ~body ~error ())
  with
  | _ -> Error (Error.parse_error ~body ())
;;

let succeed ?id value =
  let open Rensai.Ast in
  record [ "jsonrpc", string "2.0"; "id", option int id; "result", value ]
;;

let run (module H : Eff.HANDLER) ~services body =
  match from_response body with
  | Error err -> Error err
  | Ok (meth, id, params) ->
    (match List.assoc_opt meth services with
     | None -> Error (Error.method_not_found ~body ?id ~meth ())
     | Some (Handler (validator, finalizer, controller)) ->
       (try
          match validator params with
          | Error error -> Error (Error.invalid_params ~body ?id ~error ())
          | Ok params ->
            Eff.handle
              (module H)
              (fun (module H) ->
                 let now = Eff.now (module H) in
                 let context = Kohai_model.Context.make ~now in
                 let result = controller (module H) context params in
                 context, result)
            |> Result.map (fun (context, result) ->
              result |> finalizer context |> succeed ?id)
            |> Result.map_error (fun err ->
              Error.custom_to_jsonrpc ~body ?id err)
        with
        | H.Handler_exn err -> Error (Error.custom_to_jsonrpc ~body ?id err)))
;;
