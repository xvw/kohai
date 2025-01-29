(** A list of tools for easy access to working directory items. *)

(** Return the path where the list of sectors is stored. *)
val sectors : cwd:Path.t -> Path.t
