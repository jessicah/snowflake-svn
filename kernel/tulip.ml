
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

let create device =
	let IO ioaddr = device.resources.(0) in
	let out32 offset value = out32 (ioaddr + offset) value in
	let in32 offset = in32 (ioaddr + offset) in
	let eeprom_delay () = ignore (in32 Regs.csr9) in
	let read_eeprom location addr_len =
		let retval = ref zero in
		let read_cmd = location lor EE.read_cmd in
		out32 Regs.csr9 (EE.enb &! ~!EE.cs);
		out32 Regs.csr9 EE.enb;
		(* shift the read command bits out *)
		for i = 4 + addr_len downto 0 do
			let dataval = if read_cmd land (1 lsl i) > 0 then EE.data_write else zero in
			out32 Regs.csr9 (EE.enb |! dataval);
			eeprom_delay ();
			out32 Regs.csr9 (EE.enb |! dataval |! EE.shift_clk);
			eeprom_delay ();
		done;
		out32 Regs.csr9 EE.enb;
		
		for i = 16 downto 1 do
			out32 Regs.csr9 (EE.enb |! EE.shift_clk);
			eeprom_delay ();
			retval := (!retval <<! 1) |! (if (in32 Regs.csr9) &! EE.data_read > zero then one else zero);
			out32 Regs.csr9 EE.enb;
			eeprom_delay ();
		done;
		
		(* terminate the eeprom access *)
		out32 Regs.csr9 (EE.enb &! ~!EE.cs);
		!retval
	in
	
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
	(* ugh, what a mess! but need this stuff to get mac addy :( *)
	(*
        int sa_offset = 0;
        int ee_addr_size = read_eeprom(ioaddr, 0xff, 8) & 0x40000 ? 8 : 6;

        for (i = 0; i < sizeof(ee_data)/2; i++)
            ((u16 * )ee_data)[i] =
                le16_to_cpu(read_eeprom(ioaddr, i, ee_addr_size));

        /* DEC now has a specification (see Notes) but early board makers
           just put the address in the first EEPROM locations. */
        /* This does  memcmp(eedata, eedata+16, 8) */
        for (i = 0; i < 8; i ++)
            if (ee_data[i] != ee_data[16+i])
                sa_offset = 20;
        if (ee_data[0] == 0xff  &&  ee_data[1] == 0xff &&  ee_data[2] == 0) {
            sa_offset = 2;              /* Grrr, damn Matrox boards. */
        }
        for (i = 0; i < ETH_ALEN; i ++) {
            nic->node_addr[i] = ee_data[i + sa_offset];
            sum += ee_data[i + sa_offset];
        }
				
    if (sum == 0  || sum == ETH_ALEN*0xff) {
        printf("%s: EEPROM not present!\n", tp->nic_name);
        for (i = 0; i < ETH_ALEN-1; i++)
            nic->node_addr[i] = last_phys_addr[i];
        nic->node_addr[i] = last_phys_addr[i] + 1;
    }

    for (i = 0; i < ETH_ALEN; i++)
        last_phys_addr[i] = nic->node_addr[i];

    printf("%s: %! at ioaddr %hX\n", tp->nic_name, nic->node_addr, ioaddr);
	*)
	Vt100.printf "This is where driver init would go...\n"

let init () =
	DeviceManager.add_driver "Tulip 21140 NIC" create 0x1011 0x0009
