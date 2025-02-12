let list ?body ?id (module H : Eff.HANDLER) () =
  let cwd = Global.ensure_supervision ?body ?id (module H) () in
  let file = Kohai_model.Resolver.sectors ~cwd in
  let content = Eff.read_file (module H) file in
  let lexbuf = Lexing.from_string content in
  lexbuf
  |> Rensai.Lang.from_lexingbuf_to_list ~reverse:false
  |> Kohai_model.Sector.Set.from_list
;;

let save ?body ?id (module H : Eff.HANDLER) sector =
  let cwd = Global.ensure_supervision ?body ?id (module H) () in
  let file = Kohai_model.Resolver.sectors ~cwd in
  let sectors = list ?body ?id (module H : Eff.HANDLER) () in
  let sectors = Kohai_model.Sector.Set.push sector sectors in
  let content = Kohai_model.Sector.Set.dump sectors in
  let () = Eff.write_file (module H) file content in
  sectors
;;
