
(* PCI Bus *)

type device =
	{
		id : int32;
		vendor : int;
		device : int;
		resources : resource array;
		request_line : int
	}
and resource =
	| Empty
	| IO of int
	| Memory of int32

val probe : int -> int -> int -> device
val probe_bus : unit -> device list
