
(* device manager *)

open PCI

let drivers = Hashtbl.create 7

let load_driver device =
	try
		let (name, driver) = Hashtbl.find drivers (device.vendor, device.device) in
		begin try
			driver device;
            Vt100.printf "[LOADED] %04x:%04x %s\n" device.vendor device.device name
            (*Vt100.printf "%s (%04x:%04x): loaded\n" name device.vendor device.device*)
        with ex ->
            Vt100.printf "[FAILED] %04x:%04x: %s\n"
                device.vendor device.device (Printexc.to_string ex)
        end
	with Not_found -> ()

let scan_pci_bus () =
	let devices = PCI.probe_bus () in
	List.iter load_driver devices

let add_driver name init vendor device =
	Hashtbl.add drivers (vendor, device) (name, init)
