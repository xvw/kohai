(** A list of tools for easy access to working directory items. *)

(** Return the state. *)
val state : cwd:Path.t -> Path.t

(** Return the path where the list of sectors is stored. *)
val sectors : cwd:Path.t -> Path.t

(** Return the path where the list of projects is stored. *)
val projects : cwd:Path.t -> Path.t

(** Returns the folder containing logs. *)
val logs : cwd:Path.t -> Path.t

(** Returns the folder containing the list of logs. *)
val all_logs : cwd:Path.t -> Path.t

(** Return the list of last logs. *)
val last_logs : cwd:Path.t -> Path.t

(** Returns the path containing transient logs. *)
val transient_logs : cwd:Path.t -> Path.t

(** Return the path where the sector cache is stored. *)
val sector_folder : cwd:Path.t -> Path.t

(** Return the path where the project cache is stored. *)
val project_folder : cwd:Path.t -> Path.t
