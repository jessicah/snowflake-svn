
(* tulip 21140 network driver (found in virtual pc) *)

open PCI

let create device =
	Vt100.printf "This is where driver init would go..."

let init () =
	DeviceManager.add_driver "Tulip 21140 NIC" create 0x1011 0x0009
