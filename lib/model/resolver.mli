(** A list of tools for easy access to working directory items. *)

(** Return the path where the list of sectors is stored. *)
val sectors : cwd:Path.t -> Path.t

(** Return the path where the list of projects is stored. *)
val projects : cwd:Path.t -> Path.t

(** Returns the folder containing logs. *)
val logs : cwd:Path.t -> Path.t

(** Returns the path containing transient logs. *)
val transient_logs : cwd:Path.t -> Path.t
