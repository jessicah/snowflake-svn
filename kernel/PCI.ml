
(* PCI Bus *)

open Asm
open Int32

module R = struct
	let port = 0xCF8
	let data = 0xCFC
end

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
and resource =
	| Empty
	| IO of int
	| Memory of int32

exception Invalid_address_space

module AddressSpace = struct

	module IOAddress = struct
		let read8 base ofs =
			Asm.in8 (base + ofs)
		let read16 base ofs =
			Asm.in16 (base + ofs)
		let read32 base ofs =
			Asm.in32 (base + ofs)
		let write8 base ofs value =
			Asm.out8 (base + ofs) value
		let write16 base ofs value =
			Asm.out16 (base + ofs) value
		let write32 base ofs value =
			Asm.out32 (base + ofs) value
	end
	
	(* This MemAddress module is probably broken :p *)
	module MemAddress = struct
		let read8 addr ofs =
			(Asm.array8 addr ofs).{ofs}
		let read16 addr ofs =
			(Asm.array16 addr ofs).{ofs / 2}
		let read32 addr ofs =
			(Asm.array32 addr ofs).{ofs / 4}
		let write8 addr ofs value =
			(Asm.array8 addr ofs).{ofs} <- value
		let write16 addr ofs value =
			(Asm.array16 addr ofs).{ofs / 2} <- value
		let write32 addr ofs value =
			(Asm.array32 addr ofs).{ofs / 4} <- value
	end
	
	let read8 = function
	| Empty -> raise Invalid_address_space
	| IO base -> IOAddress.read8 base
	| Memory addr -> MemAddress.read8 addr
	
	let read16 = function
	| Empty -> raise Invalid_address_space
	| IO base -> IOAddress.read16 base 
	| Memory addr -> MemAddress.read16 addr 
	
	let read32  = function
	| Empty -> raise Invalid_address_space
	| IO base -> IOAddress.read32 base 
	| Memory addr -> MemAddress.read32 addr 
	
	let write8   = function
	| Empty -> raise Invalid_address_space
	| IO base -> IOAddress.write8 base 
	| Memory addr -> MemAddress.write8 addr  
	
	let write16   = function
	| Empty -> raise Invalid_address_space
	| IO base -> IOAddress.write16 base  
	| Memory addr -> MemAddress.write16 addr 
	
	let write32   = function
	| Empty -> raise Invalid_address_space
	| IO base -> IOAddress.write32 base  
	| Memory addr -> MemAddress.write32 addr  end

let aim command offset =
	let value =
		Int32.logor command (Int32.logand (Int32.lognot 3l) (Int32.of_int offset))
	in out32 R.port value

let read8 command offset =
	aim command offset;
	in8 (R.data + (offset land 3))

let read16 command offset =
	aim command offset;
	in16 (R.data + (offset land 2))

let read32 command offset =
	aim command offset;
	in32 R.data

let write8 command offset value =
	aim command offset;
	out8 (R.data + (offset land 3)) value

let write16 command offset value =
	aim command offset;
	out16 (R.data + (offset land 2)) value

let write32 command offset value =
	aim command offset;
	out32 R.data value

module C = struct
	let vendor = 0x00
	let device = 0x02
	let header_type = 0x0E
	let resource_addr = 0x10
	let resource_addr_size = 0x04
	let request_line = 0x3C
end

let probe bus device funct =
	let id =
		let x = (bus lsl 16) lor (device lsl 11) lor ((funct land 7) lsl 8)
		in Int32.logor (Int32.shift_left one 31) (Int32.of_int x)
	in
	let vendor = read16 id C.vendor in
	if vendor = 0xFFFF or vendor = 0 then
		raise Not_found;
	
	let device =
		{
			id = id;
			vendor = vendor;
			device = read16 id C.device;
			resources = Array.init 6 (fun i ->
					let resource = read32 id
							(C.resource_addr + (C.resource_addr_size * i))
						in
					if logand resource one = one then begin
						let resource = logand resource 0xFFFF_FFFEl in
						if resource = zero then Empty
						else IO (to_int resource)
					end else begin
						if resource = zero then Empty
						else Memory resource
					end);
			request_line = read8 id C.request_line;
			b = bus; d = device; f = funct;
		}
	in device

let probe_bus () =
	let list = ref [] in
	for i = 0 to 255 do
		for j = 0 to 31 do
			(*for k = 0 to 7 do*)
				try
					list := probe i j 0 :: !list
				with Not_found -> ()
			(*done*)
		done
	done;
	!list
