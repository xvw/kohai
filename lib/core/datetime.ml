type op =
  | Min of int
  | Sec of int
  | Hour of int
  | Day of int
  | Week of int
  | Month of int
  | Year of int

type duration = int64 * int64 * int64 * int64

type month =
  | Jan
  | Feb
  | Mar
  | Apr
  | May
  | Jun
  | Jul
  | Aug
  | Sep
  | Oct
  | Nov
  | Dec

type day_of_week =
  | Mon
  | Tue
  | Wed
  | Thu
  | Fri
  | Sat
  | Sun

type t =
  { year : int
  ; month : month
  ; day : int
  ; hour : int
  ; min : int
  ; sec : int
  }

let month_to_int = function
  | Jan -> 0
  | Feb -> 1
  | Mar -> 2
  | Apr -> 3
  | May -> 4
  | Jun -> 5
  | Jul -> 6
  | Aug -> 7
  | Sep -> 8
  | Oct -> 9
  | Nov -> 10
  | Dec -> 11
;;

let dow_to_int = function
  | Mon -> 0
  | Tue -> 1
  | Wed -> 2
  | Thu -> 3
  | Fri -> 4
  | Sat -> 5
  | Sun -> 6
;;

let next_month = function
  | Jan -> Feb
  | Feb -> Mar
  | Mar -> Apr
  | Apr -> May
  | May -> Jun
  | Jun -> Jul
  | Jul -> Aug
  | Aug -> Sep
  | Sep -> Oct
  | Oct -> Nov
  | Nov -> Dec
  | Dec -> Jan
;;

let prev_month = function
  | Jan -> Dec
  | Feb -> Jan
  | Mar -> Feb
  | Apr -> Mar
  | May -> Apr
  | Jun -> May
  | Jul -> Jun
  | Aug -> Jul
  | Sep -> Aug
  | Oct -> Sep
  | Nov -> Oct
  | Dec -> Nov
;;

let compare_date { year; month; day; _ } b =
  let cmp = Int.compare year b.year in
  if Int.equal cmp 0
  then (
    let cmp = Int.compare (month_to_int month) (month_to_int b.month) in
    if Int.equal cmp 0 then Int.compare day b.day else cmp)
  else cmp
;;

let compare_time { hour; min; sec; _ } b =
  let cmp = Int.compare hour b.hour in
  if Int.equal cmp 0
  then (
    let cmp = Int.compare min b.min in
    if Int.equal cmp 0 then Int.compare sec b.sec else cmp)
  else cmp
;;

let compare a b =
  let cmp = compare_date a b in
  if Int.equal cmp 0 then compare_time a b else cmp
;;

let equal a b = Int.equal (compare a b) 0
let unix = { year = 1970; month = Jan; day = 1; hour = 0; min = 0; sec = 0 }
let compare_day_of_week a b = Int.compare (dow_to_int a) (dow_to_int b)
let equal_day_of_week a b = Int.equal (compare_day_of_week a b) 0

let validate_year =
  let open Rensai.Validation in
  Int.greater_or_equal ~than:0
;;

let is_leap year = if year mod 100 = 0 then year mod 400 = 0 else year mod 4 = 0

let aux_days_in_month year month =
  match month with
  | Jan | Mar | May | Jul | Aug | Oct | Dec -> 31
  | Feb -> if is_leap year then 29 else 28
  | _ -> 30
;;

let validate_day year month =
  let open Rensai.Validation in
  let dim = aux_days_in_month year month in
  Int.in_range ~min:1 ~max:dim
;;

let validate_hour =
  let open Rensai.Validation in
  Int.in_range ~min:0 ~max:23
;;

let validate_min_sec =
  let open Rensai.Validation in
  Int.in_range ~min:0 ~max:59
;;

let validate_month = function
  | 1 -> Ok Jan
  | 2 -> Ok Feb
  | 3 -> Ok Mar
  | 4 -> Ok Apr
  | 5 -> Ok May
  | 6 -> Ok Jun
  | 7 -> Ok Jul
  | 8 -> Ok Aug
  | 9 -> Ok Sep
  | 10 -> Ok Oct
  | 11 -> Ok Nov
  | 12 -> Ok Dec
  | subject ->
    Rensai.Validation.fail_with
      ~subject:(string_of_int subject)
      "Unexpected month value"
;;

let make ?(time = 0, 0, 0) ~year ~month ~day () =
  let h, m, s = time in
  let open Rensai.Validation in
  let* year = validate_year year in
  let* day = validate_day year month day in
  let* hour = validate_hour h in
  let* min = validate_min_sec m in
  let+ sec = validate_min_sec s in
  { year; month; day; hour; min; sec }
;;

let make' ?(time = 0, 0, 0) ~year ~month ~day () =
  let open Rensai.Validation in
  let* month = validate_month month in
  make ~time ~year ~month ~day ()
;;

let from_unix ?(time = 0, 0, 0) ~year ~month ~day () =
  let year = year + 1900
  and month = succ month in
  make' ~time ~year ~month ~day ()
;;

let day_of_week { year; month; day; _ } =
  let month_value = function
    | Jan -> 0
    | Feb -> 3
    | Mar -> 3
    | Apr -> 6
    | May -> 1
    | Jun -> 4
    | Jul -> 6
    | Aug -> 2
    | Sep -> 5
    | Oct -> 0
    | Nov -> 3
    | Dec -> 5
  in
  let yy = year mod 100 in
  let cc = (year - yy) / 100 in
  let c_code = [| 6; 4; 2; 0 |].(cc mod 4) in
  let y_code = (yy + (yy / 4)) mod 7 in
  let m_code =
    let v = month_value month in
    if is_leap year && (month = Jan || month = Feb) then v - 1 else v
  in
  let index = (c_code + y_code + m_code + day) mod 7 in
  [| Sun; Mon; Tue; Wed; Thu; Fri; Sat |].(index)
;;

let pp_day_of_week st dow =
  Format.fprintf
    st
    (match dow with
     | Mon -> "mon"
     | Tue -> "tue"
     | Wed -> "wed"
     | Thu -> "thu"
     | Fri -> "fri"
     | Sat -> "sat"
     | Sun -> "sun")
;;

let pp_month st mon =
  Format.fprintf
    st
    (match mon with
     | Jan -> "jan"
     | Feb -> "feb"
     | Mar -> "mar"
     | Apr -> "apr"
     | May -> "may"
     | Jun -> "jun"
     | Jul -> "jul"
     | Aug -> "aug"
     | Sep -> "sep"
     | Oct -> "oct"
     | Nov -> "nov"
     | Dec -> "dec")
;;

let pp ?(sep = "-") () st { year; month; day; hour; min; sec } =
  Format.fprintf
    st
    "%04d%s%02d%s%02dT%02d%s%02d%s%02d"
    year
    sep
    (succ @@ month_to_int month)
    sep
    day
    hour
    sep
    min
    sep
    sec
;;

let pp_compact st { year; month; day; hour; min; sec } =
  Format.fprintf
    st
    "%04d/%02d/%02d at %02d:%02d:%02d"
    year
    (succ @@ month_to_int month)
    day
    hour
    min
    sec
;;

let pp_rfc3339 ?(tz = "Z") () st { year; month; day; hour; min; sec } =
  Format.fprintf
    st
    "%04d-%02d-%02dT%02d:%02d:%02d%s"
    year
    (succ @@ month_to_int month)
    day
    hour
    min
    sec
    tz
;;

let pp_rfc822 ?(tz = "gmt") () st ({ year; month; day; hour; min; sec } as dt) =
  let dow =
    dt
    |> day_of_week
    |> Format.asprintf "%a" pp_day_of_week
    |> String.capitalize_ascii
  in
  let mon = month |> Format.asprintf "%a" pp_month |> String.capitalize_ascii in
  Format.fprintf
    st
    "%s, %02d %s %04d %02d:%02d:%02d %s"
    dow
    day
    mon
    year
    hour
    min
    sec
    tz
;;

let pp_duration st (d, h, m, s) =
  let sum others = List.fold_left Int64.add Int64.zero others in
  let pp_with_suffix suff others st x =
    let coma = if Int64.(equal zero (sum others)) then "" else ", " in
    if Int64.(equal zero x)
    then Format.fprintf st ""
    else Format.fprintf st "%Ld%s%s" x suff coma
  in
  let pp_seconds others st x =
    if Int64.(equal zero x && not (equal (sum others) zero))
    then Format.fprintf st ""
    else Format.fprintf st "%Lds" x
  in
  Format.fprintf
    st
    "%a%a%a%a"
    (pp_with_suffix "d" [ h; m; s ])
    d
    (pp_with_suffix "h" [ m; s ])
    h
    (pp_with_suffix "m" [ s ])
    m
    (pp_seconds [ d; h; m ])
    s
;;

let days_in_month { year; month; _ } = aux_days_in_month year month
let begin_of_day dt = { dt with hour = 0; min = 0; sec = 0 }
let end_of_day dt = { dt with hour = 23; min = 59; sec = 59 }
let begin_of_month dt = { dt with day = 1 } |> begin_of_day

let end_of_month dt =
  let dim = aux_days_in_month dt.year dt.month in
  { dt with day = dim } |> end_of_day
;;

let begin_of_year dt = { dt with month = Jan; day = 1 } |> begin_of_day
let end_of_year dt = { dt with month = Dec; day = 31 } |> end_of_day
let succ_year dt = { dt with year = succ dt.year } |> begin_of_year
let pred_year dt = { dt with year = pred dt.year } |> begin_of_year

let succ_month dt =
  match dt.month with
  | Dec -> succ_year dt
  | mon -> { dt with month = next_month mon } |> begin_of_month
;;

let pred_month dt =
  match dt.month with
  | Jan -> { (pred_year dt) with month = Dec } |> begin_of_month
  | mon -> { dt with month = prev_month mon } |> begin_of_month
;;

let pred_day dt =
  if Int.equal dt.day 1
  then dt |> pred_month |> end_of_month |> begin_of_day
  else { dt with day = pred dt.day } |> begin_of_day
;;

let succ_day dt =
  let dim = aux_days_in_month dt.year dt.month in
  if Int.equal dt.day dim
  then dt |> succ_month
  else { dt with day = succ dt.day } |> begin_of_day
;;

let pred_hour dt =
  if Int.equal dt.hour 0
  then { (dt |> pred_day) with hour = 23; min = 0; sec = 0 }
  else { dt with hour = pred dt.hour; min = 0; sec = 0 }
;;

let succ_hour dt =
  if Int.equal dt.hour 23
  then dt |> succ_day |> begin_of_day
  else { dt with hour = succ dt.hour; min = 0; sec = 0 }
;;

let pred_min dt =
  if Int.equal dt.min 0
  then { (dt |> pred_hour) with min = 59; sec = 0 }
  else { dt with min = pred dt.min; sec = 0 }
;;

let succ_min dt =
  if Int.equal dt.min 59
  then { (dt |> succ_hour) with min = 0; sec = 0 }
  else { dt with min = succ dt.min; sec = 0 }
;;

let pred_sec dt =
  if Int.equal dt.sec 0
  then { (dt |> pred_min) with sec = 59 }
  else { dt with sec = pred dt.sec }
;;

let succ_sec dt =
  if Int.equal dt.sec 59
  then { (dt |> succ_min) with sec = 0 }
  else { dt with sec = succ dt.sec }
;;

let adding i fpred fsucc dt =
  let f = if i < 0 then fpred else fsucc in
  let i = Int.abs i in
  let rec aux dt n = if Int.equal i n then dt else aux (f dt) (succ n) in
  aux dt 0
;;

let begin_of_week dt =
  match day_of_week dt with
  | Mon -> begin_of_day dt
  | dow ->
    let v = dow_to_int dow in
    dt |> adding (-v) pred_day succ_day |> begin_of_day
;;

let end_of_week dt =
  match day_of_week dt with
  | Sun -> end_of_day dt
  | dow ->
    let v = dow_to_int dow in
    dt |> adding (6 - v) pred_day succ_day |> end_of_day
;;

let succ_week dt = dt |> end_of_week |> succ_day |> begin_of_day
let pred_week dt = dt |> begin_of_week |> pred_day |> begin_of_week

let seconds_of { year; month; day; hour; min; sec } =
  let m = succ (month_to_int month) in
  let m, y = if m <= 2 then m + 12, year - 1 else m, year in
  let m, y = Float.of_int m, Float.of_int y in
  (146097.0 *. y /. 400.0) +. (((153.0 *. m) +. 8.0) /. 5.0) +. Float.of_int day
  |> Int64.of_float
  |> Int64.mul 86400L
  |> Int64.(add @@ of_int sec)
  |> Int64.add @@ Int64.of_int (min * 60)
  |> Int64.add @@ Int64.of_int (hour * 3600)
;;

let seconds_to_duration x =
  let ( - ) = Int64.sub
  and ( * ) = Int64.mul in
  let d = Int64.div x 86400L in
  let h = Int64.div (x - (d * 86400L)) 3600L in
  let m = Int64.div (x - (d * 86400L) - (h * 3600L)) 60L in
  let s = x - (d * 86400L) - (h * 3600L) - (m * 60L) in
  d, h, m, s
;;

let diff a b =
  let a = seconds_of a
  and b = seconds_of b in
  Int64.abs (Int64.sub b a)
;;

let as_time dt =
  let a = seconds_of unix
  and b = seconds_of dt in
  a |> Int64.sub b |> Int64.to_float
;;

let diff_to_duration a b = diff a b |> seconds_to_duration
let min x = Min x
let sec x = Sec x
let hour x = Hour x
let day x = Day x
let month x = Month x
let year x = Year x
let week x = Week x

let callback_of = function
  | Sec x -> Int.abs x, if x < 0 then pred_sec else succ_sec
  | Min x -> Int.abs x, if x < 0 then pred_min else succ_min
  | Hour x -> Int.abs x, if x < 0 then pred_hour else succ_hour
  | Day x -> Int.abs x, if x < 0 then pred_day else succ_day
  | Month x -> Int.abs x, if x < 0 then pred_month else succ_month
  | Year x -> Int.abs x, if x < 0 then pred_year else succ_year
  | Week x -> Int.abs x, if x < 0 then pred_week else succ_week
;;

let rev_op = function
  | Min x -> Min (-x)
  | Sec x -> Sec (-x)
  | Hour x -> Hour (-x)
  | Day x -> Day (-x)
  | Month x -> Month (-x)
  | Year x -> Year (-x)
  | Week x -> Week (-x)
;;

let find_next f dow dt =
  let rec aux dt =
    let curr_dow = day_of_week dt in
    if Int.equal (dow_to_int dow) (dow_to_int curr_dow) then dt else aux (f dt)
  in
  aux (f dt)
;;

let on_next = find_next succ_day
let on_last = find_next pred_day

let add op dt =
  let i, f = callback_of op in
  let rec aux dt n = if Int.equal i n then dt else aux (f dt) (succ n) in
  aux dt 0
;;

module Infix = struct
  let ( + ) dt op = add op dt
  let ( - ) dt op = add (rev_op op) dt
  let ( = ) = equal
  let ( <> ) x y = not (equal x y)
  let ( > ) x y = compare x y > 0
  let ( >= ) x y = compare x y >= 0
  let ( < ) x y = compare x y < 0
  let ( <= ) x y = compare x y <= 0
end

let validate_datetime_from_string s =
  match
    Scanf.sscanf_opt
      s
      "%04d%c%02d%c%02d%c%02d%c%02d%c%02d"
      (fun year _ month _ day _ hour _ min _ sec ->
         (hour, min, sec), year, month, day)
  with
  | None -> Rensai.Validation.fail_with ~subject:s "is not a valid date"
  | Some (time, year, month, day) -> make' ~time ~year ~month ~day ()
;;

let validate_date_from_string s =
  match
    Scanf.sscanf_opt s "%04d%c%02d%c%02d" (fun year _ month _ day ->
      year, month, day)
  with
  | None -> Rensai.Validation.fail_with ~subject:s "is not a valid date"
  | Some (year, month, day) -> make' ~year ~month ~day ()
;;

let from_string =
  let open Rensai.Validation in
  String.trim & (validate_datetime_from_string / validate_date_from_string)
;;

let validate_from_record =
  let open Rensai.Validation in
  record (fun f ->
    let open Record in
    let+ str =
      required
        f
        "repr"
        (record (fun g ->
           let+ str = required g "regular" (string & from_string) in
           str))
    in
    str)
;;

let from_rensai =
  let open Rensai.Validation in
  validate_from_record / (string & from_string)
;;

let pp_regular = pp

let to_rensai ({ year; month; day; hour; min; sec } as dt) =
  let dow = day_of_week dt in
  let open Rensai.Ast in
  record
    [ "year", int year
    ; ( "month"
      , sum (fun month -> Format.asprintf "%a" pp_month month, unit ()) month )
    ; "month_int", int @@ (month_to_int month |> succ)
    ; "day", int day
    ; "hour", int hour
    ; "min", int min
    ; "sec", int sec
    ; ( "day_of_week"
      , sum (fun dow -> Format.asprintf "%a" pp_day_of_week dow, unit ()) dow )
    ; ( "repr"
      , record
          [ "rfc3339", string (Format.asprintf "%a" (pp_rfc3339 ()) dt)
          ; "rfc822", string (Format.asprintf "%a" (pp_rfc822 ()) dt)
          ; "regular", string (Format.asprintf "%a" (pp_regular ()) dt)
          ; "compact", string (Format.asprintf "%a" pp_compact dt)
          ] )
    ]
;;

let to_compact_rensai dt = Rensai.Ast.string @@ Format.asprintf "%a" (pp ()) dt

module Query = struct
  type nonrec t =
    | Now
    | At of int * int * int
    | Absolute of t

  let resolve datetime = function
    | None -> datetime
    | Some x ->
      (match x with
       | Now -> datetime
       | Absolute other_datetime -> other_datetime
       | At (hour, min, sec) -> { datetime with hour; min; sec })
  ;;

  let of_s r = Re.(r |> whole_string |> compile)

  module Regex = struct
    let trim r = Re.(seq [ rep blank; r; rep blank ])
    let constant k = trim Re.(no_case @@ str k)
    let time_sep = Re.set ":-/T "
    let min_or_sec = Re.(seq [ opt (rg '0' '5'); digit ])
    let hour = Re.(seq [ opt (rg '0' '2'); digit ])
    let at = Re.(seq [ rep blank; opt (no_case @@ str "at"); rep blank ])

    let time_full =
      Re.(
        seq
          [ at
          ; group hour
          ; alt
              [ no_case (char 'h')
              ; opt
                  (seq
                     [ alt [ time_sep; no_case (char 'h') ]
                     ; group min_or_sec
                     ; opt
                         (seq
                            [ alt [ time_sep; no_case (char 'm') ]
                            ; group min_or_sec
                            ])
                     ])
              ]
          ])
      |> trim
    ;;

    let opt_int_of_group g i =
      Option.bind (Re.Group.get_opt g i) int_of_string_opt
    ;;
  end

  module Opt = struct
    let ( let* ) x f = Option.bind x f
  end

  let ( <|> ) a b () =
    match a () with
    | Ok a -> Ok a
    | Error _ -> b ()
  ;;

  let as_time_full query () =
    let result =
      let open Opt in
      let* group = Re.exec_opt (of_s Regex.time_full) query in
      let* hour = Regex.opt_int_of_group group 1 in
      let min = Regex.opt_int_of_group group 2 |> Option.value ~default:0 in
      let sec = Regex.opt_int_of_group group 3 |> Option.value ~default:0 in
      if
        (hour >= 0 && hour <= 23)
        && (min >= 0 && min <= 59)
        && sec >= 0
        && sec <= 59
      then Some (hour, min, sec)
      else None
    in
    match result with
    | Some (hour, min, sec) -> Ok (At (hour, min, sec))
    | None -> Rensai.Validation.fail_with ~subject:query "Invalid query"
  ;;

  let as_now query () =
    if Re.execp (of_s @@ Regex.constant "now") query
    then Ok Now
    else Rensai.Validation.fail_with ~subject:query "Invalid query"
  ;;

  let as_time query () = as_time_full query ()

  let as_absolute query_str () =
    query_str |> from_string |> Result.map (fun x -> Absolute x)
  ;;

  let from_string query =
    (as_now query <|> as_time query <|> as_absolute query) ()
  ;;

  let from_rensai =
    let open Rensai.Validation in
    string & from_string
  ;;
end

include Infix

let min_of a b = if b > a then a else b
let max_of a b = if a < b then a else b

let pp_relative now st ({ hour; min; sec; _ } as dt) =
  let truncate_now = begin_of_day now in
  let truncate_cur = begin_of_day dt in
  let yesterday_now = pred_day now in
  let tomorrow_now = succ_day now in
  if equal truncate_cur truncate_now
  then Format.fprintf st "Today at %02d:%02d:%02d" hour min sec
  else if equal yesterday_now truncate_cur
  then Format.fprintf st "Yesterday at %02d:%02d:%02d" hour min sec
  else if equal tomorrow_now truncate_cur
  then Format.fprintf st "Tomorrow at %02d:%02d:%02d" hour min sec
  else pp_compact st dt
;;

let as_month_file ?ext ~cwd { year; month; _ } =
  let ext =
    Option.fold
      ~none:""
      ~some:(fun x ->
        match x.[0] with
        | '.' -> x
        | _ -> "." ^ x
        | exception _ -> "")
      ext
  in
  let year = year |> Format.asprintf "%04d" in
  let month = month_to_int month |> succ in
  let month = Format.asprintf "%02d%s" month ext in
  Path.(cwd / year / month)
;;
