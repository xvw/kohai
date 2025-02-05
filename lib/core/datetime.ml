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

let days_in_month year month =
  match month with
  | Jan | Mar | May | Jul | Aug | Oct | Dec -> 31
  | Feb -> if is_leap year then 29 else 28
  | _ -> 30
;;

let validate_day year month =
  let open Rensai.Validation in
  let dim = days_in_month year month in
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

let begin_of_day dt = { dt with hour = 0; min = 0; sec = 0 }
let end_of_day dt = { dt with hour = 23; min = 59; sec = 59 }
let begin_of_month dt = { dt with day = 1 } |> begin_of_day

let end_of_month dt =
  let dim = days_in_month dt.year dt.month in
  { dt with day = dim } |> end_of_day
;;
