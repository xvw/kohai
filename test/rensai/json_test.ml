(* The aim of the test suite is to perform a round-trip, losing
   information by passing through [ezjsonm] and then returning to a
   valid expression. (and it is also an experimentation about
   expressivity)*)

let round_trip t f value =
  let open Rensai.Json in
  Result.bind
    (value |> t |> to_ezjsonm |> from_ezjsonm |> f)
    (fun ezstep -> ezstep |> t |> to_yojson |> from_yojson |> f)
;;

let dump pp status =
  status
  |> Format.asprintf "%a" (Rensai.Validation.pp_checked pp)
  |> print_endline
;;

let assert_eq equal x y =
  assert (Rensai.Validation.equal_checked equal (Ok x) y)
;;

module Activation_status = struct
  type t =
    | Active
    | Nonactive

  let pp state status =
    Format.fprintf
      state
      "%s"
      (match status with
       | Active -> "Active"
       | Nonactive -> "Nonactive")
  ;;

  let equal a b =
    match a, b with
    | Active, Active | Nonactive, Nonactive -> true
    | Active, _ | Nonactive, _ -> false
  ;;

  let to_rs =
    let open Rensai.Ast in
    sum (function
      | Active -> "active", unit ()
      | Nonactive -> "nonactive", unit ())
  ;;

  let from_rs =
    let open Rensai.Validation in
    sum
      [ "active", unitish & const Active
      ; "nonactive", unitish & const Nonactive
      ]
  ;;

  let round_trip = round_trip to_rs from_rs
  let dump = dump pp
  let assert_eq = assert_eq equal

  let%expect_test "roundtrip for activation-status - 1" =
    let status = Active in
    let result = status |> round_trip in
    let _ = assert_eq status result in
    dump result;
    [%expect {| Active |}]
  ;;

  let%expect_test "roundtrip for activation-status - 2" =
    let status = Nonactive in
    let result = status |> round_trip in
    let _ = assert_eq status result in
    dump result;
    [%expect {| Nonactive |}]
  ;;
end

module Gender = struct
  type t =
    | Male
    | Female
    | Other of string

  let pp state gender =
    Format.fprintf
      state
      "%s"
      (match gender with
       | Male -> "Male"
       | Female -> "Female"
       | Other s -> "Other: " ^ s)
  ;;

  let equal a b =
    match a, b with
    | Male, Male | Female, Female -> true
    | Other a, Other b -> String.equal a b
    | Male, _ | Female, _ | Other _, _ -> false
  ;;

  let to_rs =
    let open Rensai.Ast in
    sum (function
      | Male -> "male", unit ()
      | Female -> "female", unit ()
      | Other s -> "other", string s)
  ;;

  let from_rs =
    let open Rensai.Validation in
    sum
      [ "male", unitish & const Male
      ; "female", unitish & const Female
      ; ("other", string & String.is_not_blank $ fun x -> Other x)
      ]
  ;;

  let round_trip = round_trip to_rs from_rs
  let dump = dump pp
  let assert_eq = assert_eq equal

  let%expect_test "roundtrip for gender - 1" =
    let gender = Male in
    let result = gender |> round_trip in
    let _ = assert_eq gender result in
    dump result;
    [%expect {| Male |}]
  ;;

  let%expect_test "roundtrip for gender - 2" =
    let gender = Female in
    let result = gender |> round_trip in
    let _ = assert_eq gender result in
    dump result;
    [%expect {| Female |}]
  ;;

  let%expect_test "roundtrip for gender - 3" =
    let gender = Other "something else" in
    let result = gender |> round_trip in
    let _ = assert_eq gender result in
    dump result;
    [%expect {| Other: something else |}]
  ;;

  let%expect_test "roundtrip for gender - 4" =
    let gender = Other "" in
    let result = gender |> round_trip in
    dump result;
    [%expect
      {|
      {message: "unexpected value";
       error: "`` is blank"}
      |}]
  ;;

  let%expect_test "roundtrip for gender - 5" =
    let gender = Other "         " in
    let result = gender |> round_trip in
    dump result;
    [%expect
      {|
      {message: "unexpected value";
       error: "`         ` is blank"}
      |}]
  ;;
end

module Identity = struct
  type t =
    { username : string
    ; first_name : string option
    ; last_name : string option
    ; age : int option
    ; email : string option
    }

  let make ?first_name ?last_name ?age ?email ~username () =
    { username; first_name; last_name; age; email }
  ;;

  let pp =
    Fmt.Dump.(
      record
        [ field "username" (fun { username; _ } -> username) string
        ; field
            "first_name"
            (fun { first_name; _ } -> first_name)
            (option string)
        ; field "last_name" (fun { last_name; _ } -> last_name) (option string)
        ; field "age" (fun { age; _ } -> age) (option Fmt.int)
        ; field "email" (fun { email; _ } -> email) (option string)
        ])
  ;;

  let equal { username; first_name; last_name; age; email } other =
    String.equal username other.username
    && Option.equal String.equal first_name other.first_name
    && Option.equal String.equal last_name other.last_name
    && Option.equal Int.equal age other.age
    && Option.equal String.equal email other.email
  ;;

  let to_rs { username; first_name; last_name; age; email } =
    let open Rensai.Ast in
    record
      [ "username", string username
      ; "first_name", option string first_name
      ; "last_name", option string last_name
      ; "age", option int age
      ; "email", option string email
      ]
  ;;

  let is_name =
    let open Rensai.Validation in
    string & String.is_not_blank & String.capitalize
  ;;

  let is_email =
    let open Rensai.Validation in
    string
    & String.where
        ~message:(Fmt.str "`%a` does not look to an email")
        (fun str ->
           match Stdlib.String.split_on_char '@' str with
           | [ _; domain ] ->
             Stdlib.List.length (Stdlib.String.split_on_char '.' domain) > 1
           | _ -> false)
  ;;

  let from_rs =
    let open Rensai.Validation in
    record (fun fields ->
      let open Record in
      let+ username = required fields "username" is_name
      and+ first_name = optional fields "first_name" is_name
      and+ last_name = optional fields "last_name" is_name
      and+ age = optional fields "age" (int & Int.is_positive)
      and+ email = optional fields "email" is_email in
      { username; first_name; last_name; age; email })
  ;;

  let dump = dump pp
  let round_trip = round_trip to_rs from_rs
  let assert_eq = assert_eq equal

  let%expect_test "roundtrip for identity - 1" =
    let identity = make ~username:"Xvw" () in
    let result = identity |> round_trip in
    let _ = assert_eq identity result in
    dump result;
    [%expect
      {|
      { "username" = "Xvw";
        "first_name" = None;
        "last_name" = None;
        "age" = None;
        "email" = None }
      |}]
  ;;

  let%expect_test "roundtrip for identity - 2" =
    let identity = make ~username:"Xvw" ~first_name:"Xavier" () in
    let result = identity |> round_trip in
    let _ = assert_eq identity result in
    dump result;
    [%expect
      {|
      { "username" = "Xvw";
        "first_name" = Some "Xavier";
        "last_name" = None;
        "age" = None;
        "email" = None }
      |}]
  ;;

  let%expect_test "roundtrip for identity - 2" =
    let identity = make ~username:"Xvw" ~first_name:"   " () in
    let result = identity |> round_trip in
    dump result;
    [%expect
      {|
      {message: "unexpected record";
       where:
        [{message: "invalid field";
          field: "first_name";
          where: {message: "unexpected value";
                  error: "`   ` is blank"}}];
       value:
        {age = <null>; email = <null>; first_name = "   "; last_name = <null>;
          username = "Xvw"}}
      |}]
  ;;

  let%expect_test "roundtrip for identity - 3" =
    let identity =
      make ~username:"Xvw" ~first_name:"Xavier" ~last_name:"Vdw" ()
    in
    let result = identity |> round_trip in
    let _ = assert_eq identity result in
    dump result;
    [%expect
      {|
      { "username" = "Xvw";
        "first_name" = Some "Xavier";
        "last_name" = Some "Vdw";
        "age" = None;
        "email" = None }
      |}]
  ;;

  let%expect_test "roundtrip for identity - 4" =
    let identity =
      make ~username:"Xvw" ~first_name:"Xavier" ~last_name:"Vdw" ~age:35 ()
    in
    let result = identity |> round_trip in
    let _ = assert_eq identity result in
    dump result;
    [%expect
      {|
      { "username" = "Xvw";
        "first_name" = Some "Xavier";
        "last_name" = Some "Vdw";
        "age" = Some 35;
        "email" = None }
      |}]
  ;;

  let%expect_test "roundtrip for identity - 5" =
    let identity =
      make
        ~username:"Xvw"
        ~first_name:"Xavier"
        ~last_name:"Vdw"
        ~age:35
        ~email:"xavier@mail.com"
        ()
    in
    let result = identity |> round_trip in
    let _ = assert_eq identity result in
    dump result;
    [%expect
      {|
      { "username" = "Xvw";
        "first_name" = Some "Xavier";
        "last_name" = Some "Vdw";
        "age" = Some 35;
        "email" = Some "xavier@mail.com" }
      |}]
  ;;

  let%expect_test "roundtrip for identity - 6" =
    let identity =
      make
        ~username:"Xvw"
        ~first_name:"Xavier"
        ~last_name:"Vdw"
        ~age:35
        ~email:"xaviermail.com"
        ()
    in
    let result = identity |> round_trip in
    dump result;
    [%expect
      {|
      {message: "unexpected record";
       where:
        [{message: "invalid field";
          field: "email";
          where:
           {message: "unexpected value";
            error: "`\"xaviermail.com\"` does not look to an email"}}];
       value:
        {age = 35; email = "xaviermail.com"; first_name = "Xavier";
          last_name = "Vdw"; username = "Xvw"}}
      |}]
  ;;

  let%expect_test "roundtrip for identity - 7" =
    let identity =
      make
        ~username:"Xvw"
        ~first_name:"Xavier"
        ~last_name:"Vdw"
        ~age:35
        ~email:"xavier@mailcom"
        ()
    in
    let result = identity |> round_trip in
    dump result;
    [%expect
      {|
      {message: "unexpected record";
       where:
        [{message: "invalid field";
          field: "email";
          where:
           {message: "unexpected value";
            error: "`\"xavier@mailcom\"` does not look to an email"}}];
       value:
        {age = 35; email = "xavier@mailcom"; first_name = "Xavier";
          last_name = "Vdw"; username = "Xvw"}}
      |}]
  ;;

  let%expect_test "roundtrip for identity - 8" =
    let identity =
      make
        ~username:"Xvw"
        ~first_name:"Xavier"
        ~last_name:"Vdw"
        ~age:35
        ~email:"xavier@mail.co.uk"
        ()
    in
    let result = identity |> round_trip in
    let _ = assert_eq identity result in
    dump result;
    [%expect
      {|
      { "username" = "Xvw";
        "first_name" = Some "Xavier";
        "last_name" = Some "Vdw";
        "age" = Some 35;
        "email" = Some "xavier@mail.co.uk" }
      |}]
  ;;
end

type user =
  { identity : Identity.t
  ; status : Activation_status.t
  ; gender : Gender.t
  ; has_checked_rules : bool
  ; note : string option
  ; ratings : float list
  ; big_number : int64
  }

let make
      ?(has_checked_rules = false)
      ?note
      ?(ratings = [])
      ?(big_number = Int64.max_int)
      ~identity
      ~status
      ~gender
      ()
  =
  { identity; status; gender; has_checked_rules; note; ratings; big_number }
;;

let pp =
  Fmt.Dump.(
    record
      [ field "identity" (fun { identity = x; _ } -> x) Identity.pp
      ; field "status" (fun { status = x; _ } -> x) Activation_status.pp
      ; field "gender" (fun { gender = x; _ } -> x) Gender.pp
      ; field
          "has_checked_rules"
          (fun { has_checked_rules = x; _ } -> x)
          Fmt.bool
      ; field "note" (fun { note = x; _ } -> x) (option string)
      ; field "ratings" (fun { ratings = x; _ } -> x) (list Fmt.float)
      ; field "big_number" (fun { big_number = x; _ } -> x) Fmt.int64
      ])
;;

let equal
      { identity; status; gender; has_checked_rules; note; ratings; big_number }
      other
  =
  Identity.equal identity other.identity
  && Activation_status.equal status other.status
  && Gender.equal gender other.gender
  && Bool.equal has_checked_rules other.has_checked_rules
  && Option.equal String.equal note other.note
  && List.equal Float.equal ratings other.ratings
  && Int64.equal big_number other.big_number
;;

let to_rs
      { identity; status; gender; has_checked_rules; note; ratings; big_number }
  =
  let open Rensai.Ast in
  record
    [ "identity", Identity.to_rs identity
    ; "status", Activation_status.to_rs status
    ; "gender", Gender.to_rs gender
    ; "has_checked_rules", bool has_checked_rules
    ; "note", option string note
    ; "ratings", list float ratings
    ; "big_number", int64 big_number
    ]
;;

let from_rs =
  let open Rensai.Validation in
  record (fun fields ->
    let open Record in
    let+ identity = required fields "identity" Identity.from_rs
    and+ status = required fields "status" Activation_status.from_rs
    and+ gender = required fields "gender" Gender.from_rs
    and+ has_checked_rules = required fields "has_checked_rules" bool
    and+ note = optional fields "note" (string & String.not_equal "Banned")
    and+ ratings = optional_or ~default:[] fields "ratings" (list_of float)
    and+ big_number = required fields "big_number" int64 in
    { identity; status; gender; has_checked_rules; note; ratings; big_number })
;;

let dump = dump pp
let round_trip = round_trip to_rs from_rs
let assert_eq = assert_eq equal

let%expect_test "roundtrip for activation-status - 1" =
  let user =
    let identity =
      Identity.make ~username:"Xvw" ~first_name:"Xavier" ~last_name:"Vdw" ()
    in
    make ~identity ~gender:Gender.Male ~status:Activation_status.Nonactive ()
  in
  let result = user |> round_trip in
  let _ = assert_eq user result in
  dump result;
  [%expect
    {|
    { "identity" =
       { "username" = "Xvw";
         "first_name" = Some "Xavier";
         "last_name" = Some "Vdw";
         "age" = None;
         "email" = None };
      "status" = Nonactive;
      "gender" = Male;
      "has_checked_rules" = false;
      "note" = None;
      "ratings" = [];
      "big_number" = 9223372036854775807 }
    |}]
;;

let%expect_test "roundtrip for activation-status - 2" =
  let user =
    let identity =
      Identity.make
        ~username:"Xvw"
        ~first_name:"Xavier"
        ~last_name:"Vdw"
        ~age:35
        ~email:"xav@kohai.io"
        ()
    in
    make
      ~identity
      ~gender:Gender.Male
      ~status:Activation_status.Nonactive
      ~has_checked_rules:true
      ~ratings:[ 3.14; 42.42; 100.0 ]
      ~note:"Administrator"
      ()
  in
  let result = user |> round_trip in
  let _ = assert_eq user result in
  dump result;
  [%expect
    {|
    { "identity" =
       { "username" = "Xvw";
         "first_name" = Some "Xavier";
         "last_name" = Some "Vdw";
         "age" = Some 35;
         "email" = Some "xav@kohai.io" };
      "status" = Nonactive;
      "gender" = Male;
      "has_checked_rules" = true;
      "note" = Some "Administrator";
      "ratings" = [3.14; 42.42; 100];
      "big_number" = 9223372036854775807 }
    |}]
;;

let%expect_test "roundtrip for activation-status - 3" =
  let user =
    let identity =
      Identity.make
        ~username:"   "
        ~first_name:"   "
        ~last_name:"Vdw"
        ~age:35
        ~email:"xav@kohai"
        ()
    in
    make
      ~identity
      ~gender:Gender.Male
      ~status:Activation_status.Nonactive
      ~has_checked_rules:true
      ~ratings:[ 3.14; 42.42; 100.0 ]
      ~note:"Administrator"
      ()
  in
  let result = user |> round_trip in
  dump result;
  [%expect
    {|
    {message: "unexpected record";
     where:
      [{message: "invalid field";
        field: "identity";
        where:
         {message: "unexpected record";
          where:
           [{message: "invalid field";
             field: "username";
             where: {message: "unexpected value";
                     error: "`   ` is blank"}};
            {message: "invalid field";
             field: "first_name";
             where: {message: "unexpected value";
                     error: "`   ` is blank"}};
            {message: "invalid field";
             field: "email";
             where:
              {message: "unexpected value";
               error: "`\"xav@kohai\"` does not look to an email"}}];
          value:
           {age = 35; email = "xav@kohai"; first_name = "   "; last_name = "Vdw";
             username = "   "}}}];
     value:
      {big_number = 9223372036854775807; gender = male(<null>);
        has_checked_rules = true;
        identity =
         {age = 35; email = "xav@kohai"; first_name = "   "; last_name = "Vdw";
           username = "   "};
        note = "Administrator"; ratings = [3.14; 42.42; 100];
        status = nonactive(<null>)}}
    |}]
;;

let%expect_test "roundtrip for activation-status - 4" =
  let user =
    let identity =
      Identity.make
        ~username:"   "
        ~first_name:"   "
        ~last_name:"Vdw"
        ~age:35
        ~email:"xav@kohai"
        ()
    in
    make
      ~identity
      ~gender:Gender.Male
      ~status:Activation_status.Nonactive
      ~has_checked_rules:true
      ~ratings:[ 3.14; 42.42; 100.0 ]
      ~note:"Banned"
      ()
  in
  let result = user |> round_trip in
  dump result;
  [%expect
    {|
    {message: "unexpected record";
     where:
      [{message: "invalid field";
        field: "identity";
        where:
         {message: "unexpected record";
          where:
           [{message: "invalid field";
             field: "username";
             where: {message: "unexpected value";
                     error: "`   ` is blank"}};
            {message: "invalid field";
             field: "first_name";
             where: {message: "unexpected value";
                     error: "`   ` is blank"}};
            {message: "invalid field";
             field: "email";
             where:
              {message: "unexpected value";
               error: "`\"xav@kohai\"` does not look to an email"}}];
          value:
           {age = 35; email = "xav@kohai"; first_name = "   "; last_name = "Vdw";
             username = "   "}}};
       {message: "invalid field";
        field: "note";
        where:
         {message: "unexpected value";
          error: "`a` (\"Banned\") is equal to `b` (\"Banned\")"}}];
     value:
      {big_number = 9223372036854775807; gender = male(<null>);
        has_checked_rules = true;
        identity =
         {age = 35; email = "xav@kohai"; first_name = "   "; last_name = "Vdw";
           username = "   "};
        note = "Banned"; ratings = [3.14; 42.42; 100];
        status = nonactive(<null>)}}
    |}]
;;
