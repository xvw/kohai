let dump rensai_expr =
  rensai_expr |> Format.asprintf "%a" Rensai_fmt.pp_ast |> print_endline
;;

open Rensai.Ast

let%expect_test "pretty-print null" =
  let expr = null () in
  dump expr;
  [%expect {| <null> |}]
;;

let%expect_test "pretty-print unit" =
  let expr = unit () in
  dump expr;
  [%expect {| <unit> |}]
;;

let%expect_test "pretty-print true" =
  let expr = bool true in
  dump expr;
  [%expect {| true |}]
;;

let%expect_test "pretty-print false" =
  let expr = bool false in
  dump expr;
  [%expect {| false |}]
;;

let%expect_test "pretty-print char" =
  let expr = char 'a' in
  dump expr;
  [%expect {| 'a' |}]
;;

let%expect_test "pretty-print int" =
  let expr = int 42 in
  dump expr;
  [%expect {| 42 |}]
;;

let%expect_test "pretty-print negative int" =
  let expr = int (-42) in
  dump expr;
  [%expect {| -42 |}]
;;

let%expect_test "pretty-print int32" =
  let expr = int32 42l in
  dump expr;
  [%expect {| 42l |}]
;;

let%expect_test "pretty-print negative int32" =
  let expr = int32 (-42l) in
  dump expr;
  [%expect {| -42l |}]
;;

let%expect_test "pretty-print int64" =
  let expr = int64 42L in
  dump expr;
  [%expect {| 42L |}]
;;

let%expect_test "pretty-print negative int64" =
  let expr = int64 (-42L) in
  dump expr;
  [%expect {| -42L |}]
;;

let%expect_test "pretty-print float" =
  let expr = float 3.1234 in
  dump expr;
  [%expect {| 3.1234 |}]
;;

let%expect_test "pretty-print negative float" =
  let expr = float (-3.14) in
  dump expr;
  [%expect {| -3.14 |}]
;;

let%expect_test "pretty-print an empty string" =
  let expr = string "" in
  dump expr;
  [%expect {| "" |}]
;;

let%expect_test "pretty-print a string" =
  let expr = string "Hello, World!" in
  dump expr;
  [%expect {| "Hello, World!" |}]
;;

let%expect_test "pretty-print a long string" =
  let expr =
    string
      "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Suspendisse \
       tincidunt imperdiet congue. Morbi in dui consectetur, malesuada neque \
       a, porttitor dui. Curabitur tellus augue, facilisis laoreet dictum id, \
       lacinia sed dui. Integer vulputate faucibus condimentum. Aenean \
       vehicula dictum nibh, sit amet semper nibh condimentum vel. Quisque \
       tempor euismod urna, non rutrum ligula egestas id. Quisque vitae \
       venenatis ex, vitae dictum massa. Suspendisse bibendum dolor eu elit \
       lobortis rutrum. Ut ut volutpat lectus, vel laoreet dui. Cras at lectus \
       nec nisi fringilla convallis eget at mauris. Ut non lorem ultricies, \
       vulputate dolor a, ultricies est. In dapibus urna diam. Mauris elit \
       odio, maximus et posuere id, rutrum nec velit. Mauris egestas facilisis \
       porta. Etiam blandit leo ex. Ut elementum blandit vehicula."
  in
  dump expr;
  [%expect
    {| "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Suspendisse tincidunt imperdiet congue. Morbi in dui consectetur, malesuada neque a, porttitor dui. Curabitur tellus augue, facilisis laoreet dictum id, lacinia sed dui. Integer vulputate faucibus condimentum. Aenean vehicula dictum nibh, sit amet semper nibh condimentum vel. Quisque tempor euismod urna, non rutrum ligula egestas id. Quisque vitae venenatis ex, vitae dictum massa. Suspendisse bibendum dolor eu elit lobortis rutrum. Ut ut volutpat lectus, vel laoreet dui. Cras at lectus nec nisi fringilla convallis eget at mauris. Ut non lorem ultricies, vulputate dolor a, ultricies est. In dapibus urna diam. Mauris elit odio, maximus et posuere id, rutrum nec velit. Mauris egestas facilisis porta. Etiam blandit leo ex. Ut elementum blandit vehicula." |}]
;;

let%expect_test "pretty-print an empty list" =
  let expr = list string [] in
  dump expr;
  [%expect {| [] |}]
;;

let%expect_test "pretty-print a list" =
  let expr = list string [ "hello"; "world" ] in
  dump expr;
  [%expect {| ["hello"; "world"] |}]
;;

let%expect_test "pretty-print a big list" =
  let expr = list int (List.init 200 Fun.id) in
  dump expr;
  [%expect
    {|
    [0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15; 16; 17; 18; 19; 20;
     21; 22; 23; 24; 25; 26; 27; 28; 29; 30; 31; 32; 33; 34; 35; 36; 37; 38; 39;
     40; 41; 42; 43; 44; 45; 46; 47; 48; 49; 50; 51; 52; 53; 54; 55; 56; 57; 58;
     59; 60; 61; 62; 63; 64; 65; 66; 67; 68; 69; 70; 71; 72; 73; 74; 75; 76; 77;
     78; 79; 80; 81; 82; 83; 84; 85; 86; 87; 88; 89; 90; 91; 92; 93; 94; 95; 96;
     97; 98; 99; 100; 101; 102; 103; 104; 105; 106; 107; 108; 109; 110; 111;
     112; 113; 114; 115; 116; 117; 118; 119; 120; 121; 122; 123; 124; 125; 126;
     127; 128; 129; 130; 131; 132; 133; 134; 135; 136; 137; 138; 139; 140; 141;
     142; 143; 144; 145; 146; 147; 148; 149; 150; 151; 152; 153; 154; 155; 156;
     157; 158; 159; 160; 161; 162; 163; 164; 165; 166; 167; 168; 169; 170; 171;
     172; 173; 174; 175; 176; 177; 178; 179; 180; 181; 182; 183; 184; 185; 186;
     187; 188; 189; 190; 191; 192; 193; 194; 195; 196; 197; 198; 199]
    |}]
;;

let%expect_test "pretty-print an hlist" =
  let expr =
    hlist
      [ int 23
      ; bool true
      ; quad'
          int
          string
          int64
          (list bool)
          42
          "foo"
          33445556L
          [ true; true; false ]
      ]
  in
  dump expr;
  [%expect
    {| [23; true; (42, ("foo", (33445556L, [true; true; false])))] |}]
;;

let%expect_test "pretty-print a simple record" =
  let expr =
    record
      [ "age", option int (Some 42)
      ; "activated", either string int (Left "foo")
      ; ( "sub"
        , record
            [ "is_valid", result bool bool (Error false)
            ; ( "a_long_string"
              , string
                  "Vivamus quis felis sit amet nunc pretium aliquet. \
                   Suspendisse a magna ut nisl sodales blandit sed et mi. \
                   Quisque fermentum hendrerit lectus ac pulvinar. Duis \
                   euismod magna et magna convallis viverra. Pellentesque \
                   laoreet luctus pellentesque. Sed sagittis viverra leo, quis \
                   auctor nisi cursus ac. Nam cursus urna et tincidunt \
                   gravida. Ut eget ipsum in massa sagittis vestibulum eget at \
                   est. Vestibulum bibendum mattis quam, sit amet tincidunt \
                   ligula. Aliquam at tempus augue. Aenean ac est urna. Nullam \
                   iaculis, dolor i" )
            ; ( "another_random_list"
              , list
                  (fun x ->
                     record
                       [ "x", string x
                       ; "up", string (String.uppercase_ascii x)
                       ])
                  (List.init 43 (fun i -> String.make i 'a')) )
            ] )
      ; "a_random_list", list int (List.init 320 Fun.id)
      ]
  in
  dump expr;
  [%expect
    {|
    {a_random_list =
      [0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15; 16; 17; 18; 19; 20;
       21; 22; 23; 24; 25; 26; 27; 28; 29; 30; 31; 32; 33; 34; 35; 36; 37; 38;
       39; 40; 41; 42; 43; 44; 45; 46; 47; 48; 49; 50; 51; 52; 53; 54; 55; 56;
       57; 58; 59; 60; 61; 62; 63; 64; 65; 66; 67; 68; 69; 70; 71; 72; 73; 74;
       75; 76; 77; 78; 79; 80; 81; 82; 83; 84; 85; 86; 87; 88; 89; 90; 91; 92;
       93; 94; 95; 96; 97; 98; 99; 100; 101; 102; 103; 104; 105; 106; 107; 108;
       109; 110; 111; 112; 113; 114; 115; 116; 117; 118; 119; 120; 121; 122;
       123; 124; 125; 126; 127; 128; 129; 130; 131; 132; 133; 134; 135; 136;
       137; 138; 139; 140; 141; 142; 143; 144; 145; 146; 147; 148; 149; 150;
       151; 152; 153; 154; 155; 156; 157; 158; 159; 160; 161; 162; 163; 164;
       165; 166; 167; 168; 169; 170; 171; 172; 173; 174; 175; 176; 177; 178;
       179; 180; 181; 182; 183; 184; 185; 186; 187; 188; 189; 190; 191; 192;
       193; 194; 195; 196; 197; 198; 199; 200; 201; 202; 203; 204; 205; 206;
       207; 208; 209; 210; 211; 212; 213; 214; 215; 216; 217; 218; 219; 220;
       221; 222; 223; 224; 225; 226; 227; 228; 229; 230; 231; 232; 233; 234;
       235; 236; 237; 238; 239; 240; 241; 242; 243; 244; 245; 246; 247; 248;
       249; 250; 251; 252; 253; 254; 255; 256; 257; 258; 259; 260; 261; 262;
       263; 264; 265; 266; 267; 268; 269; 270; 271; 272; 273; 274; 275; 276;
       277; 278; 279; 280; 281; 282; 283; 284; 285; 286; 287; 288; 289; 290;
       291; 292; 293; 294; 295; 296; 297; 298; 299; 300; 301; 302; 303; 304;
       305; 306; 307; 308; 309; 310; 311; 312; 313; 314; 315; 316; 317; 318; 319];
     activated = (left("foo")); age = 42;
     sub =
      {a_long_string =
        "Vivamus quis felis sit amet nunc pretium aliquet. Suspendisse a magna ut nisl sodales blandit sed et mi. Quisque fermentum hendrerit lectus ac pulvinar. Duis euismod magna et magna convallis viverra. Pellentesque laoreet luctus pellentesque. Sed sagittis viverra leo, quis auctor nisi cursus ac. Nam cursus urna et tincidunt gravida. Ut eget ipsum in massa sagittis vestibulum eget at est. Vestibulum bibendum mattis quam, sit amet tincidunt ligula. Aliquam at tempus augue. Aenean ac est urna. Nullam iaculis, dolor i";
       another_random_list =
        [{up = ""; x = ""}; {up = "A"; x = "a"}; {up = "AA"; x = "aa"};
         {up = "AAA"; x = "aaa"}; {up = "AAAA"; x = "aaaa"};
         {up = "AAAAA"; x = "aaaaa"}; {up = "AAAAAA"; x = "aaaaaa"};
         {up = "AAAAAAA"; x = "aaaaaaa"}; {up = "AAAAAAAA"; x = "aaaaaaaa"};
         {up = "AAAAAAAAA"; x = "aaaaaaaaa"};
         {up = "AAAAAAAAAA"; x = "aaaaaaaaaa"};
         {up = "AAAAAAAAAAA"; x = "aaaaaaaaaaa"};
         {up = "AAAAAAAAAAAA"; x = "aaaaaaaaaaaa"};
         {up = "AAAAAAAAAAAAA"; x = "aaaaaaaaaaaaa"};
         {up = "AAAAAAAAAAAAAA"; x = "aaaaaaaaaaaaaa"};
         {up = "AAAAAAAAAAAAAAA"; x = "aaaaaaaaaaaaaaa"};
         {up = "AAAAAAAAAAAAAAAA"; x = "aaaaaaaaaaaaaaaa"};
         {up = "AAAAAAAAAAAAAAAAA"; x = "aaaaaaaaaaaaaaaaa"};
         {up = "AAAAAAAAAAAAAAAAAA"; x = "aaaaaaaaaaaaaaaaaa"};
         {up = "AAAAAAAAAAAAAAAAAAA"; x = "aaaaaaaaaaaaaaaaaaa"};
         {up = "AAAAAAAAAAAAAAAAAAAA"; x = "aaaaaaaaaaaaaaaaaaaa"};
         {up = "AAAAAAAAAAAAAAAAAAAAA"; x = "aaaaaaaaaaaaaaaaaaaaa"};
         {up = "AAAAAAAAAAAAAAAAAAAAAA"; x = "aaaaaaaaaaaaaaaaaaaaaa"};
         {up = "AAAAAAAAAAAAAAAAAAAAAAA"; x = "aaaaaaaaaaaaaaaaaaaaaaa"};
         {up = "AAAAAAAAAAAAAAAAAAAAAAAA"; x = "aaaaaaaaaaaaaaaaaaaaaaaa"};
         {up = "AAAAAAAAAAAAAAAAAAAAAAAAA"; x = "aaaaaaaaaaaaaaaaaaaaaaaaa"};
         {up = "AAAAAAAAAAAAAAAAAAAAAAAAAA"; x = "aaaaaaaaaaaaaaaaaaaaaaaaaa"};
         {up = "AAAAAAAAAAAAAAAAAAAAAAAAAAA"; x = "aaaaaaaaaaaaaaaaaaaaaaaaaaa"};
         {up = "AAAAAAAAAAAAAAAAAAAAAAAAAAAA";
          x = "aaaaaaaaaaaaaaaaaaaaaaaaaaaa"};
         {up = "AAAAAAAAAAAAAAAAAAAAAAAAAAAAA";
          x = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaa"};
         {up = "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAA";
          x = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"};
         {up = "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA";
          x = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"};
         {up = "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA";
          x = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"};
         {up = "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA";
          x = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"};
         {up = "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA";
          x = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"};
         {up = "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA";
          x = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"};
         {up = "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA";
          x = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"};
         {up = "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA";
          x = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"};
         {up = "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA";
          x = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"};
         {up = "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA";
          x = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"};
         {up = "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA";
          x = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"};
         {up = "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA";
          x = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"};
         {up = "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA";
          x = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"}];
       is_valid = (error(false))}}
    |}]
;;

type gender =
  | Male
  | Female
  | Other of string

type user =
  { first_name : string
  ; last_name : string
  ; is_active : bool
  ; gender : gender
  ; crowd : user list
  }

let user ?(crowd = []) ?(is_active = true) first_name last_name gender =
  { first_name; last_name; is_active; gender; crowd }
;;

let conv_gender =
  constr (function
    | Male -> "male", unit ()
    | Female -> "female", unit ()
    | Other s -> "other", string s)
;;

let rec conv_user { first_name; last_name; is_active; gender; crowd } =
  record
    [ "first_name", string first_name
    ; "last_name", string last_name
    ; "is_active", bool is_active
    ; "gender", conv_gender gender
    ; "crowd", list conv_user crowd
    ]
;;

let%expect_test "test creating an user - 1" =
  let expr = conv_user @@ user "A" "B" Male in
  dump expr;
  [%expect
    {|
    {crowd = []; first_name = "A"; gender = (male(<unit>)); is_active = true;
     last_name = "B"}
    |}]
;;

let%expect_test "test creating an user - 2" =
  let expr =
    conv_user
      (user
         ~crowd:[ user "C" "D" Male; user "E" "F" Male ]
         ~is_active:false
         "A"
         "B"
         Female)
  in
  dump expr;
  [%expect
    {|
    {crowd =
      [{crowd = []; first_name = "C"; gender = (male(<unit>)); is_active = true;
        last_name = "D"};
       {crowd = []; first_name = "E"; gender = (male(<unit>)); is_active = true;
        last_name = "F"}];
     first_name = "A"; gender = (female(<unit>)); is_active = false;
     last_name = "B"}
    |}]
;;
