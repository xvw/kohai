let rensai obj =
  let s = obj |> Rensai.Json.to_yojson |> Yojson.Safe.to_string in
  let len = String.length s in
  "Content-Length: " ^ string_of_int len ^ "\r\n\r\n" ^ s
;;

let rensai_error message =
  let obj = Error.unknown_error message () in
  rensai (obj |> Error.to_rensai)
;;

let handler (module H : Eff.HANDLER) body =
  match Eff.handle (module H) (Jsonrpc.run ~services:Services.all body) with
  | Ok result -> rensai result
  | Error err -> rensai (err |> Error.to_rensai)
;;

let parse p flow ~max_size =
  let buf = Eio.Buf_read.of_flow flow ~max_size in
  Eio.Buf_read.format_errors p buf
;;

let input_parser =
  let open Eio.Buf_read.Syntax in
  let* () = Eio.Buf_read.string "Content-Length: " in
  let* len =
    Eio.Buf_read.take_while (function
      | '0' .. '9' -> true
      | _ -> false)
  in
  let len =
    match int_of_string_opt len with
    | Some len -> len
    | None -> failwith ("Invalid length " ^ len)
  in
  let* _ = Eio.Buf_read.char '\r' in
  let* _ = Eio.Buf_read.char '\n' in
  let* _ = Eio.Buf_read.char '\r' in
  let* _ = Eio.Buf_read.char '\n' in
  Eio.Buf_read.take len
;;

let run (module H : Eff.HANDLER) env =
  let stdin = Eio.Stdenv.stdin env in
  let stdout = Eio.Stdenv.stdout env in
  let rec aux () =
    let () =
      match parse ~max_size:1024 input_parser stdin with
      | Ok res ->
        let r = handler (module H) res in
        Eio.Buf_write.with_flow stdout (fun w -> Eio.Buf_write.string w r)
      | Error (`Msg message) ->
        let error = rensai_error message in
        Eio.Buf_write.with_flow stdout (fun w -> Eio.Buf_write.string w error)
    in
    aux ()
  in
  aux ()
;;
