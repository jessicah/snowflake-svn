
(* PCI Bus *)

type device_id = int32

type device =
	{
		id : device_id;
		vendor : int;
		device : int;
		resources : resource array;
		request_line : int;
		b : int;
		d : int;
		f : int;
	}
and resource
	
exception Invalid_address_space

module AddressSpace : sig

	val read8 : resource -> int -> int
	val read16 : resource -> int -> int
	val read32 : resource -> int -> int32
	val write8 : resource -> int -> int -> unit
	val write16 : resource -> int -> int -> unit
	val write32 : resource -> int -> int32 -> unit
	
end

val probe : int -> int -> int -> device
val probe_bus : unit -> device list

val read8 : device_id -> int -> int
val read16 : device_id -> int -> int
val read32 : device_id -> int -> int32
val write8 : device_id -> int -> int -> unit
val write16 : device_id -> int -> int -> unit
val write32 : device_id -> int -> int32 -> unit
