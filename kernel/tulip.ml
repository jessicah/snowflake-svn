
(* tulip 21140 network driver (found in virtual pc) *)

let init () =
	DeviceManager.add_driver "Tulip 21140 NIC" ignore 0x1011 0x0009
