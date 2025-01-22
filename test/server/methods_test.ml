open Kohai_server

let dump = function
  | Ok value -> Fmt.str "%a" Rensai.Ast.pp value
  | Error error -> error |> Error.to_rensai |> Fmt.str "%a" Rensai.Ast.pp
;;

module Handler = Kohai_core.Eff.Handler (Virtfs.Make (struct
    let fs = Virtfs.(from_list [])
  end))

let input ?(id = 1) ?params meth =
  Format.asprintf
    {j|{"jsonrpc": "2.0", "method": "%s", "id": %d%s}|j}
    meth
    id
    (match params with
     | None -> ""
     | Some x -> Format.asprintf {|, "params": %s|} x)
;;

let%expect_test "ensure supervision - not supervised" =
  let input = input ~id:42 "kohai/ensure_supervision" in
  input
  |> Jsonrpc.run ~services:Services.all
  |> Kohai_core.Eff.handle (module Handler)
  |> dump
  |> print_endline;
  [%expect
    {|
    {error =
      {code = -32000; data = "No supervised directory for the current session";
        input =
         "{\"jsonrpc\": \"2.0\", \"method\": \"kohai/ensure_supervision\", \"id\": 42}";
        message = "Server error"};
      id = 42; jsonrpc = "2.0"}
    |}]
;;
