open Core

type 'a t = 'a IO.t
type _ Effect.t += K_fail_with : Error.t -> 'a Effect.t

let fail_with err = IO.perform @@ K_fail_with err

let from_validated to_err = function
  | Ok x -> IO.return x
  | Error err -> fail_with @@ to_err err
;;

let parse_error () = fail_with @@ Error.parse_error ()

let method_not_found ?id meth () =
  fail_with @@ Error.method_not_found ~data:meth ?id ()
;;
