
(* partition reading code *)

val code_to_string : int -> string

type partition = {
	code: int;
	start: int;
	length: int;
}

val partitions : (int -> int -> string) -> partition list

val wrap_read : (int -> int -> string) -> partition -> int -> int -> string
val wrap_write : (int -> int -> string -> unit) -> partition -> int -> int -> string -> unit
