module R = Kohai_model.Resolver
module L = Kohai_model.Log
module S = Kohai_model.State

let update_state (module H : Eff.HANDLER) cwd log =
  let start_date = L.start_date log
  and end_date = L.end_date log in
  let state_file = R.state ~cwd in
  let content = Eff.read_file (module H) state_file in
  content
  |> S.from_string
  |> S.patch_date_boundaries start_date
  |> S.patch_date_boundaries end_date
  |> S.dump
  |> Eff.write_file (module H) state_file
;;

let get ?body ?id (module H : Eff.HANDLER) () =
  let cwd = Global.ensure_supervision ?body ?id (module H) () in
  let file = R.state ~cwd in
  let content = Eff.read_file (module H) file in
  content |> S.from_string
;;
