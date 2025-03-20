type t = Kohai_core.Datetime.t

let month_to_yocaml = function
  | Kohai_core.Datetime.Jan -> 1
  | Feb -> 2
  | Mar -> 3
  | Apr -> 4
  | May -> 5
  | Jun -> 6
  | Jul -> 7
  | Aug -> 8
  | Sep -> 9
  | Oct -> 10
  | Nov -> 11
  | Dec -> 12
;;

let to_yocaml_aux Kohai_core.Datetime.{ year; month; day; hour; min; sec } =
  Yocaml.Datetime.make
    ~time:(hour, min, sec)
    ~year
    ~month:(month_to_yocaml month)
    ~day
    ()
;;

let validate =
  let open Yocaml.Data.Validation in
  string
  & fun x ->
  match Kohai_core.Datetime.from_string x with
  | Ok dt -> Ok dt
  | Error _ -> fail_with ~given:x "Invalid datetime"
;;

let to_yocaml dt =
  match to_yocaml_aux dt with
  | Ok x -> x
  | Error _ -> failwith "kohai_datetime: should not happen"
;;

let normalize dt = dt |> to_yocaml |> Yocaml.Datetime.normalize
