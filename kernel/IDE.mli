
val init : unit -> unit

type t

val read_disk : t -> int -> int -> string

type controller = Primary | Secondary
type device = Master | Slave

val disk_to_id : controller -> device -> t

(* raises Not_found *)
val get : controller -> device -> t

type device_t = {
	read : int -> int -> string;
	write : int -> int -> string -> unit;
}

val disks : device_t array
