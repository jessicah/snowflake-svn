
(* tulip 21140 network driver (found in virtual pc) *)

open PCI

module Int32Ops = struct
	let ( +! ) = Int32.add
	let ( -! ) = Int32.sub
	let ( *! ) = Int32.mul
	let ( /! ) = Int32.div
	let ( &! ) = Int32.logand
	let ( |! ) = Int32.logor
	let ( >>! ) = Int32.shift_right_logical
	let ( <<! ) = Int32.shift_left
	let ( ~! ) = Int32.lognot
	let ( ^! ) = Int32.logxor
	let zero = Int32.zero
	let one = Int32.one
	let minus_one = Int32.minus_one
	let neg = Int32.neg
	let rem = Int32.rem
	let of_int = Int32.of_int
end

open Int32Ops
open Asm

module Regs = struct
	let csr0 = 0x00
	let csr1 = 0x08
	let csr2 = 0x10
	let csr3 = 0x18
	let csr4 = 0x20
	let csr5 = 0x28
	let csr6 = 0x30
	let csr7 = 0x38
	let csr8 = 0x40
	let csr9 = 0x48
	let csr10 = 0x50
	let csr11 = 0x58
	let csr12 = 0x60
	let csr13 = 0x68
	let csr14 = 0x70
	let csr15 = 0x78
	let csr16 = 0x80
	let csr20 = 0xA0
end

module EE = struct
	let addrlen = 6
	let size = 128
	
	let write_cmd = 5 lsl addrlen
	let read_cmd = 6 lsl addrlen
	let erase_cmd = 7 lsl addrlen
	
	let shift_clk = 0x02l
	let cs = 0x01l
	let data_write = 0x04l
	let write_0 = 0x01l
	let write_1 = 0x05l
	let data_read = 0x08l
	let enb = 0x4800l |! cs
end

(* stuff from tulip.c to note:
	
	TULIP_IOTYPE = PCI_USES_MASTER | PCI_USES_IO | PCI_USES_ADDR0
	
	DC21140 = 2
	
	HAS_MII | HAS_MEDIA_TABLE | CSR12_IN_SROM
*)

let print_mac () arr =
	String.concat ":" (List.map (Printf.sprintf "%02x") (Array.to_list arr))

(* a slightly more functional eeprom_read function *)
let rec accum count f acc = if count = 0 then acc else accum (count - 1) f (f acc)

let eeprom_delay ioaddr = ignore (in32 (ioaddr + Regs.csr9))

let read_eeprom ioaddr location addr_len =
	let read_cmd = location lor EE.read_cmd in
	let ee_addr = ioaddr + Regs.csr9 in
	out32 ee_addr (EE.enb &! ~!EE.cs);
	out32 ee_addr EE.enb;
	(* shift the read command bits out *)
	for i = 4 + addr_len downto 0 do
		let dataval = if read_cmd land (1 lsl i) > 0 then EE.data_write else zero in
		out32 ee_addr (EE.enb |! dataval);
		eeprom_delay ioaddr;
		out32 ee_addr (EE.enb |! dataval |! EE.shift_clk);
		eeprom_delay ioaddr;
	done;
	out32 ee_addr EE.enb;
	(* read the data *)
	let retval = accum 16 begin fun r ->
			out32 ee_addr (EE.enb |! EE.shift_clk);
			eeprom_delay ioaddr;
			let r = (r lsl 1) lor (if (in32 ee_addr) &! EE.data_read > zero then 1 else 0) in
			out32 ee_addr EE.enb;
			eeprom_delay ioaddr;
			r end 0
		in (* terminate the eeprom access *)
	out32 ee_addr (EE.enb &! ~!EE.cs);
	retval

let create device =
	let ioaddr = match device.resources.(0) with IO x -> x | _ -> raise Not_found in
	let out32 offset value = out32 (ioaddr + offset) value in
	let in32 offset = in32 (ioaddr + offset) in	
	(* adjust_pci_device(pci) *)
	(* disable interrupts *)
	out32 Regs.csr7 zero;
	(* stop the chip's Tx and Rx processes *)
	out32 Regs.csr6 (in32 Regs.csr6 &! ~!0x00002002l);
	(* clear the missed-packet counter *)
	ignore (in32 Regs.csr8);
	Debug.printf "%s: Looking for Tulip Chip: Vendor=%X  Device=%X\n" "DEC Tulip 21140 Fast" device.vendor device.device;
	if in32 Regs.csr5 = minus_one then begin
		Debug.printf "%s: The Tulip chip at %X is not functioning\n" "DEC Tulip 21140 Fast" ioaddr;
		raise Not_found;
	end;
	(* pcibios_read_config_byte(pci->bus, pci_devfn, PCI_REVISION, &chip_rev) *)
	Debug.printf "%s: Vendor=%X  Device=%X\n" "DEC Tulip 21140 Fast" device.vendor device.device;
	(* A serial EEPROM interface; we read now and sort it out later *)
	let ee_data = Array.make EE.size 0 in
	let sa_offset = ref 0 in
	let ee_addr_size = if (read_eeprom ioaddr 0xff 8) land 0x40000 > 0 then 8 else 6 in
	(* read the eeprom data *)
	for i = 0 to EE.size / 2 - 1 do
		let x = read_eeprom ioaddr i ee_addr_size in
		ee_data.(i * 2) <- x land 0xFF;
		ee_data.(i * 2 + 1) <- x asr 8;
	done;
	(* find the mac address *)
	for i = 0 to 7 do
		if ee_data.(i) <> ee_data.(16+i) then sa_offset := 20;
	done;
	let mac_addr = Array.sub ee_data !sa_offset 6 (* eth_alen *) in
	let sum = Array.fold_right (+) mac_addr 0 in
	if sum = 0 or sum = 6*0xFF then begin
		Debug.printf "%s: EEPROM not present!\n" "DEC 21140 Tulip Fast";
		raise Not_found;
	end;
	Vt100.printf "%s: %a at ioaddr %04x\n" "DEC 21140 Tulip Fast" print_mac mac_addr ioaddr;
	(* remainder of init code to come... :P *)
	Vt100.printf "This is where driver init would go...\n"

let init () =
	DeviceManager.add_driver "Tulip 21140 NIC" create 0x1011 0x0009
