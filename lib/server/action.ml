(* let set_supervised_directory ?id (module H : Eff.HANDLER) = function *)
(*   | None -> Eff.set_supervised_directory (module H) None *)
(*   | Some path  *)
(* ;; *)

let ensure_supervision body ?id (module H : Eff.HANDLER) () =
  match Eff.get_supervised_directory (module H) with
  | None -> Eff.raise (module H) @@ Error.no_supervised_directory ?id ~body ()
  | Some _ -> ()
;;
