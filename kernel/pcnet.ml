
open PCI
open Bigarray

(* AMD PCNET II driver in Word I/O Mode *)

type t = {
	pci : device;
	base : int; (* the base address for port I/O *)
}

module C = struct
	let aprom = 0x00 (* Address PROM: 16 bytes *)
	let rdp = 0x10 (* CSR data port *)
	let rap = 0x12 (* register address port (CSR+BCR) *)
	let reset = 0x14 (* for software reset *)
	let bdp = 0x16 (* BCR data port *)
end

(* Reading/Writing the Control & Status Registers (CSR) *)
let read_csr base addr =
	Asm.out16 (base + C.rap) addr;
	Asm.in16  (base + C.rdp)

let write_csr base addr value =
	Asm.out16 (base + C.rap) addr;
	Asm.out16 (base + C.rdp) value

(* Reading/Writing the Bus Control Registers (BCR) *)
let read_bcr base addr =
	Asm.out16 (base + C.rap) addr;
	Asm.in16 (base + C.bdp)

let write_bcr base addr value =
	Asm.out16 (base + C.rap) addr;
	Asm.out16 (base + C.bdp) value

let create dev =
	let base = match dev.resources.(0) with
		| IO addr -> addr
		| _ -> failwith "pcnet: expected port i/o address"
	in
	()

let init () =
	DeviceManager.add_driver "AMD PCNET II" create 0x1022 0x2000
