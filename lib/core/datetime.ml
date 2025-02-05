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

type t =
  { year : int
  ; month : month
  ; day : int
  ; hour : int
  ; min : int
  ; sec : int
  }

let unix = { year = 1970; month = Jan; day = 1; hour = 0; min = 0; sec = 0 }

let make ?(time = 0, 0, 0) ~year ~month ~day () =
  let hour, min, sec = time in
  let open Rensai.Validation.Syntax in
  assert false
;;
