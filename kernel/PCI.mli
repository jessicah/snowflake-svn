
(* PCI Bus *)

type device =
	{
		id : int32;
		vendor : int;
		device : int;
		resources : resource array;
		request_line : int;
		b : int;
		d : int;
		f : int;
	}
and resource =
	| Empty
	| IO of int
	| Memory of int32

val probe : int -> int -> int -> device
val probe_bus : unit -> device list

val read8 : int32 -> int -> int
val read16 : int32 -> int -> int
val read32 : int32 -> int -> int32
val write8 : int32 -> int -> int -> unit
val write16 : int32 -> int -> int -> unit
val write32 : int32 -> int -> int32 -> unit
