let request_input ?(id = 1) ?params meth =
  Format.asprintf
    {j|{"jsonrpc": "2.0", "method": "%s", "id": %d%s}|j}
    meth
    id
    (match params with
     | None -> ""
     | Some x -> Format.asprintf {|, "params": %s|} x)
;;

let request_dump = function
  | Ok value -> Fmt.str "%a" Rensai.Ast.pp value
  | Error error ->
    error |> Kohai_server.Error.to_rensai |> Fmt.str "%a" Rensai.Ast.pp
;;

let dump_result ?(should_fail = false) result =
  match result with
  | Ok result when not should_fail ->
    result |> Format.asprintf "[OK]: %a" Rensai.Lang.pp
  | Ok result -> result |> Format.asprintf "[ERROR]: %a" Rensai.Lang.pp
  | Error err when should_fail ->
    err
    |> Kohai_server.Error.to_rensai
    |> Format.asprintf "[OK]: %a" Rensai.Lang.pp
  | Error err ->
    err
    |> Kohai_server.Error.to_rensai
    |> Format.asprintf "[ERROR]: %a" Rensai.Lang.pp
;;

let print_result ?should_fail result =
  result |> dump_result ?should_fail |> print_endline
;;

let request ~id ?params meth =
  let i = !id in
  let () = incr id in
  request_input ~id:i ?params meth
;;

let call (module H : Kohai_core.Eff.HANDLER) ~id ?params meth =
  meth
  |> request
       ~id
       ?params:
         (Option.map
            (fun x -> x |> Rensai.Json.to_yojson |> Yojson.Safe.to_string)
            params)
  |> Kohai_server.Jsonrpc.run ~services:Kohai_server.Services.all
  |> Kohai_core.Eff.handle (module H)
;;

let call_supervise (module H : Kohai_core.Eff.HANDLER) ~id ~path () =
  let params = Rensai.Ast.string path in
  "kohai/supervision/set" |> call (module H) ~id ~params
;;

let call_supervise_get (module H : Kohai_core.Eff.HANDLER) ~id () =
  "kohai/supervision/get" |> call (module H) ~id
;;

let call_sector_list (module H : Kohai_core.Eff.HANDLER) ~id () =
  "kohai/sector/list" |> call (module H) ~id
;;

let call_project_list (module H : Kohai_core.Eff.HANDLER) ~id () =
  "kohai/project/list" |> call (module H) ~id
;;

let call_sector_save (module H : Kohai_core.Eff.HANDLER) ~id ~name ?desc () =
  let params =
    let open Rensai.Ast in
    record [ "name", string name; "description", option string desc ]
  in
  "kohai/sector/save" |> call (module H) ~id ~params
;;

let call_project_save (module H : Kohai_core.Eff.HANDLER) ~id ~name ?desc () =
  let params =
    let open Rensai.Ast in
    record [ "name", string name; "description", option string desc ]
  in
  "kohai/project/save" |> call (module H) ~id ~params
;;

let step
      (module H : Kohai_core.Eff.HANDLER)
      ?should_fail
      ?(desc = "No description")
      ~id
      callback
  =
  let _ = desc in
  let req = callback (module H : Kohai_core.Eff.HANDLER) ~id () in
  print_result ?should_fail req
;;
