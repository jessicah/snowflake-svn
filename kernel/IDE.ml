(*     IDE driver for Snowflake     *)
(* Copyright (c) 2009 Andrew Wilcox *)
(*    Released under BSD license    *)
(* Some code stolen from AWOS, also *)
(*    released under BSD license    *)

(* can use to detect IDE controllers *)
open PCI

(* begin leet hax from ICH0 *)
(* two sets of port i/o spaces *)
type io_space = {
	read8   : int -> int;
	read16  : int -> int;
	read32  : int -> int32;
	write8  : int -> int -> unit;
	write16 : int -> int -> unit;
	write32 : int -> int32 -> unit;
}

let make_io_space addr = {
	read8 = begin fun x -> Asm.in8 (addr + x) end;
	read16 = begin fun x -> Asm.in16 (addr + x) end;
	read32 = begin fun x -> Asm.in32 (addr + x) end;
	write8 = begin fun x -> Asm.out8 (addr + x) end;
	write16 = begin fun x -> Asm.out16 (addr + x) end;
	write32 = begin fun x -> Asm.out32 (addr + x) end;
}

(* end leet hax from ICH0 *)

module R = struct
	let data = 0x00
	let error = 0x01
	let sectorcount = 0x02
	let sectorstart = 0x03
	let cyl_low = 0x04
	let cyl_high = 0x05
	let head = 0x06
	let command = 0x07
	let status = 0x07
end

let swab s =
	for i = 0 to String.length s /2 -1 do
		let t = s.[i*2] in
			s.[i*2] <- s.[i*2+1];
			s.[i*2+1] <- t;
	done

let io = make_io_space (0x1f0)

let create device =
	Vt100.printf "Found an IDE controller! woo!\n"
	
let ideisr() =
	io.read8 R.status (* reading status reg clears interrupt *)

let init() =
	DeviceManager.add_driver "Generic IDE Controller: Intel PIIX4" create 0x8086 0x7111;
	DeviceManager.add_driver "Generic IDE Controller: Intel ICH0" create 0x8086 0x2411;
	DeviceManager.add_driver "Generic IDE Controller: Intel ICH0" create 0x8086 0x2421