let dump = function
  | Some rensai_expr ->
    rensai_expr |> Format.asprintf "Some %a" Rensai.Ast.pp |> print_endline
  | None -> print_endline "None"
;;

let dump_list res =
  res |> Format.asprintf "%a" (Fmt.Dump.list Rensai.Lang.pp) |> print_endline
;;

let dump_yojson res =
  res
  |> Format.asprintf "%a" Fmt.Dump.(list Yojson.Safe.pretty_print)
  |> print_endline
;;

let%expect_test "Lang.from_string - 1" =
  let input = {rs||rs} in
  input |> Rensai.Lang.from_string |> dump;
  [%expect {| None |}]
;;

let%expect_test "Lang.from_string - 2" =
  let input = {rs|()|rs} in
  input |> Rensai.Lang.from_string |> dump;
  [%expect {| Some <unit> |}]
;;

let%expect_test "Lang.from_string - 3" =
  let input = {rs|123|rs} in
  input |> Rensai.Lang.from_string |> dump;
  [%expect {| Some 123 |}]
;;

let%expect_test "Lang.from_string - 4" =
  let input = {rs|123.65|rs} in
  input |> Rensai.Lang.from_string |> dump;
  [%expect {| Some 123.65 |}]
;;

let%expect_test "Lang.from_string - 5" =
  let input = {rs|["foo", 'f']|rs} in
  input |> Rensai.Lang.from_string |> dump;
  [%expect {| Some ["foo"; 'f'] |}]
;;

let%expect_test "Lang.from_string - 5" =
  let input =
    {rs|<
     id: "foo";
     length: 42;
     isActive: true;
     gender: #Male(());
     lList: #Cons((true, #Nil(())));
     sub: <hd: [1, 2, 3]; tl:(true, (1, false))>>|rs}
  in
  input |> Rensai.Lang.from_string |> dump;
  [%expect
    {|
    Some {gender = male(<unit>); id = "foo"; isActive = true;
           lList = cons((true, nil(<unit>))); length = 42;
           sub = {hd = [1; 2; 3]; tl = (true, (1, false))}}
    |}]
;;

let%expect_test "Read multiple expressionm from lexing buf - 1" =
  let input =
    {rs|
     <foo: 1; bar: true>
     <x: #foo(1); desc:"foo bar baz">
  |rs}
  in
  let lexbuf = Lexing.from_string input in
  let result = Rensai.Lang.from_lexingbuf_to_list ~reverse:true lexbuf in
  result |> dump_list;
  [%expect {| [<bar: true; foo: 1>; <desc: "foo bar baz"; x: #foo(1)>] |}]
;;

let%expect_test "Pathologic case with duration" =
  let input =
    {|<duration: null; index: 0; label: "Try to rewrite some stuff";
  project: "kohai"; sectors: ["programming"];
  start_date: "2025-02-11T11-32-47">
<duration: null; index: 1; label: "Some drawing"; project: null;
  sectors: ["visual"]; start_date: "2025-02-11T11-33-02">
<duration: null; index: 2; label: "Une expérience"; project: null;
  sectors: ["programming"]; start_date: "2025-02-11T11-59-53">
<duration: null; index: 3; label: "Some experience"; project: "kohai";
  sectors: ["test"]; start_date: "2025-02-11T12-01-12">
<duration: null; index: 4; label: "Some experience using music";
  project: null; sectors: ["music"]; start_date: "2025-02-11T12-15-15">
<duration: null; index: 5; label: "Some sectors experiences";
  project: "capsule"; sectors: ["hacking"];
  start_date: "2025-02-11T12-15-56">
<duration: null; index: 6; label: "test"; project: null; sectors: ["foobar"];
  start_date: "2025-02-11T12-17-18">
<duration: 5612; index: 7; label: "sa;d;asld sa;ld as;ld";
  project: "preface"; sectors: ["lisp"]; start_date: "2025-02-11T12-18-31">
<duration: null; index: 8; label: "experience for fun an profit";
  project: "kohai"; sectors: ["lisp"]; start_date: "2025-02-11T12-19-43">
<duration: 4481; index: 9; label: "asdasd"; project: "asdasd";
              sectors: ["foobar"]; start_date: "2025-02-11T12-36-52">|}
  in
  let lexbuf = Lexing.from_string input in
  let result = Rensai.Lang.from_lexingbuf_to_list ~reverse:true lexbuf in
  result |> List.map Rensai.Json.to_yojson |> dump_yojson;
  [%expect
    {|
    [{
       "duration": null,
       "index": 0,
       "label": "Try to rewrite some stuff",
       "project": "kohai",
       "sectors": [ "programming" ],
       "start_date": "2025-02-11T11-32-47"
     };
     {
       "duration": null,
       "index": 1,
       "label": "Some drawing",
       "project": null,
       "sectors": [ "visual" ],
       "start_date": "2025-02-11T11-33-02"
     };
     {
       "duration": null,
       "index": 2,
       "label": "Une expérience",
       "project": null,
       "sectors": [ "programming" ],
       "start_date": "2025-02-11T11-59-53"
     };
     {
       "duration": null,
       "index": 3,
       "label": "Some experience",
       "project": "kohai",
       "sectors": [ "test" ],
       "start_date": "2025-02-11T12-01-12"
     };
     {
       "duration": null,
       "index": 4,
       "label": "Some experience using music",
       "project": null,
       "sectors": [ "music" ],
       "start_date": "2025-02-11T12-15-15"
     };
     {
       "duration": null,
       "index": 5,
       "label": "Some sectors experiences",
       "project": "capsule",
       "sectors": [ "hacking" ],
       "start_date": "2025-02-11T12-15-56"
     };
     {
       "duration": null,
       "index": 6,
       "label": "test",
       "project": null,
       "sectors": [ "foobar" ],
       "start_date": "2025-02-11T12-17-18"
     };
     {
       "duration": 5612,
       "index": 7,
       "label": "sa;d;asld sa;ld as;ld",
       "project": "preface",
       "sectors": [ "lisp" ],
       "start_date": "2025-02-11T12-18-31"
     };
     {
       "duration": null,
       "index": 8,
       "label": "experience for fun an profit",
       "project": "kohai",
       "sectors": [ "lisp" ],
       "start_date": "2025-02-11T12-19-43"
     };
     {
       "duration": 4481,
       "index": 9,
       "label": "asdasd",
       "project": "asdasd",
       "sectors": [ "foobar" ],
       "start_date": "2025-02-11T12-36-52"
     }]
    |}]
;;
