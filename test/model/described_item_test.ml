open Kohai_model

let dump x =
  x
  |> Described_item.Set.to_rensai
  |> Format.asprintf "%a" Rensai.Lang.pp
  |> print_endline
;;

let dump_ok x =
  x
  |> Result.map Described_item.Set.to_rensai
  |> Format.asprintf "%a" (Rensai.Validation.pp_checked Rensai.Lang.pp)
  |> print_endline
;;

let from_string subject =
  subject
  |> Lexing.from_string
  |> Rensai.Lang.from_lexingbuf_to_list
  |> Described_item.Set.from_list
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
    [<counter: 0; description: "about learning"; name: "learning">,
     <counter: 0; description: null; name: "programming">]
    |}]
;;

let%expect_test "Push a new sector - 1" =
  let sector = Rensai.Ast.(record [ "name", string "art" ]) in
  let result =
    let open Rensai.Validation.Syntax in
    let+ sector = Described_item.from_rensai sector in
    Described_item.Set.push sector a_sector_list
  in
  dump_ok result;
  [%expect
    {|
    [<counter: 0; description: null; name: "art">,
     <counter: 0; description: "about learning"; name: "learning">,
     <counter: 0; description: null; name: "programming">]
    |}]
;;

let%expect_test "Push a new sector - 2" =
  let sector =
    Rensai.Ast.(
      record [ "name", string "art"; "description", string "A description" ])
  in
  let result =
    let open Rensai.Validation.Syntax in
    let+ sector = Described_item.from_rensai sector in
    Described_item.Set.push sector a_sector_list
  in
  dump_ok result;
  [%expect
    {|
    [<counter: 0; description: "A description"; name: "art">,
     <counter: 0; description: "about learning"; name: "learning">,
     <counter: 0; description: null; name: "programming">]
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
    let+ sector = Described_item.from_rensai sector in
    Described_item.Set.push sector a_sector_list
  in
  dump_ok result;
  [%expect
    {|
    [<counter: 0; description: "about learning"; name: "learning">,
     <counter: 0; description: "A programming description"; name: "programming">]
    |}]
;;

let%expect_test "Push a new sector - 4" =
  let sector = Rensai.Ast.(record [ "name", string "learning" ]) in
  let result =
    let open Rensai.Validation.Syntax in
    let+ sector = Described_item.from_rensai sector in
    Described_item.Set.push sector a_sector_list
  in
  dump_ok result;
  [%expect
    {|
    [<counter: 0; description: "about learning"; name: "learning">,
     <counter: 0; description: null; name: "programming">]
    |}]
;;
