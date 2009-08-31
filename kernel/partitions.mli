
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

type partition_t = {
	info : partition;
	read : int -> int -> string;
	write : int -> int -> string -> unit;
}

val partitions_t : (int -> int -> string) -> (int -> int -> string -> unit) -> partition_t list
