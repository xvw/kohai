open Kohai_model

let dump x =
  x
  |> Sector.Set.to_rensai
  |> Format.asprintf "%a" Rensai.Lang.pp
  |> print_endline
;;

let dump_ok x =
  x
  |> Result.map Sector.Set.to_rensai
  |> Format.asprintf "%a" (Rensai.Validation.pp_checked Rensai.Lang.pp)
  |> print_endline
;;

let from_string subject =
  subject
  |> Lexing.from_string
  |> Rensai.Lang.from_lexingbuf_to_list
  |> Sector.Set.from_list
;;

let a_sector_list =
  [ {|<name: "programming">|}
  ; {|<name: "learning"; description: "about learning">|}
  ]
  |> String.concat "\n"
  |> from_string
;;

let%expect_test "Simply dump a list of sectors" =
  a_sector_list |> dump;
  [%expect
    {|
    [<description: "about learning"; name: "learning">,
     <description: null; name: "programming">]
    |}]
;;

let%expect_test "Push a new sector - 1" =
  let sector = Rensai.Ast.(record [ "name", string "art" ]) in
  let result =
    let open Rensai.Validation.Syntax in
    let+ sector = Sector.from_rensai sector in
    Sector.Set.push sector a_sector_list
  in
  dump_ok result;
  [%expect
    {|
    [<description: null; name: "art">,
     <description: "about learning"; name: "learning">,
     <description: null; name: "programming">]
    |}]
;;

let%expect_test "Push a new sector - 2" =
  let sector =
    Rensai.Ast.(
      record [ "name", string "art"; "description", string "A description" ])
  in
  let result =
    let open Rensai.Validation.Syntax in
    let+ sector = Sector.from_rensai sector in
    Sector.Set.push sector a_sector_list
  in
  dump_ok result;
  [%expect
    {|
    [<description: "A description"; name: "art">,
     <description: "about learning"; name: "learning">,
     <description: null; name: "programming">]
    |}]
;;

let%expect_test "Push a new sector - 3" =
  let sector =
    Rensai.Ast.(
      record
        [ "name", string "programming"
        ; "description", string "A programming description"
        ])
  in
  let result =
    let open Rensai.Validation.Syntax in
    let+ sector = Sector.from_rensai sector in
    Sector.Set.push sector a_sector_list
  in
  dump_ok result;
  [%expect
    {|
    [<description: "about learning"; name: "learning">,
     <description: "A programming description"; name: "programming">]
    |}]
;;

let%expect_test "Push a new sector - 4" =
  let sector = Rensai.Ast.(record [ "name", string "learning" ]) in
  let result =
    let open Rensai.Validation.Syntax in
    let+ sector = Sector.from_rensai sector in
    Sector.Set.push sector a_sector_list
  in
  dump_ok result;
  [%expect
    {|
    [<description: "about learning"; name: "learning">,
     <description: null; name: "programming">]
    |}]
;;
