type t = Kohai_model.Log.t

let entity_name = "Kohai.Log"
let neutral = Yocaml.Metadata.required entity_name

let validate =
  let open Yocaml.Data.Validation in
  record (fun f ->
    let+ start_date = required f "start_date" Datetime.validate
    and+ duration = required f "duration" Duration.validate
    and+ project = optional f "project" string
    and+ sector = required f "sector" string
    and+ label = required f "label" string
    and+ meta =
      optional_or
        ~default:(Kohai_model.Key_value.empty ())
        f
        "meta"
        (Key_value.validate string)
    and+ links =
      optional_or
        ~default:(Kohai_model.Key_value.empty ())
        f
        "links"
        (Key_value.validate Url.validate)
    and+ id = required f "id" Uuid.validate in
    Kohai_model.Log.make
      ~start_date
      ~duration
      ?project
      ~sector
      ~label
      ~meta
      ~links
      ~id
      ())
;;

let normalize log =
  let open Yocaml.Data in
  let s, p = Kohai_model.Log.sector_and_project log
  and meta = Kohai_model.Log.meta log
  and links = Kohai_model.Log.links log in
  record
    [ "start_date", Datetime.normalize (Kohai_model.Log.start_date log)
    ; "end_date", Datetime.normalize (Kohai_model.Log.end_date log)
    ; "duration", Duration.normalize (Kohai_model.Log.duration log)
    ; "project", option string p
    ; "sector", string s
    ; "label", string (Kohai_model.Log.label log)
    ; "meta", Key_value.normalize string meta
    ; "links", Key_value.normalize Url.normalize links
    ; "has_project", bool @@ Option.is_some p
    ; "has_meta", bool @@ not (Kohai_model.Key_value.is_empty meta)
    ; "has_links", bool @@ not (Kohai_model.Key_value.is_empty links)
    ]
;;
