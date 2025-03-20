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

let opt_int_of_group g i = Option.bind (Re.Group.get_opt g i) int_of_string_opt
