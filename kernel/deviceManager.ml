
(* device manager *)

open PCI

let drivers = Hashtbl.create 7

let load_driver device =
	try
		let (name, driver) = Hashtbl.find drivers (device.vendor, device.device) in
		Vt100.printf "Loading %s...\r\n" name;
		driver device
	with Not_found -> ()
    | ex -> Vt100.printf "Failure loading driver: %s\r\n"
        (Printexc.to_string ex)

let scan_pci_bus () =
	let devices = PCI.probe_bus () in
	List.iter load_driver devices

let add_driver name init vendor device =
	Hashtbl.add drivers (vendor, device) (name, init)
