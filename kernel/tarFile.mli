
(* Tar File *)

type t

val open_tar_file : string -> t

val read_file : t -> string -> string

val dir_list : t -> string -> string list
