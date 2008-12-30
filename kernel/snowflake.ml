
let echo_shell () =
	while true do
		Vt100.printf "%c" (Keyboard.get_char ())
	done

let print_device dev =
	Vt100.printf "PCI: %04X:%04X\r\n" dev.PCI.vendor dev.PCI.device

open PCI

let () =
	Vga.init (); (* set up a pretty console font *)
	Keyboard.init (); (* set up the keyboard handler *)
	Tulip.init (); (* unfortunately it won't get linked in otherwise... *)
	Vt100.printf "Hello, from ML :)\nUsing ocaml version: %s\n" Sys.ocaml_version;
	Asm.sti ();
	(*let pci_devices = PCI.probe_bus () in
	List.iter print_device pci_devices;*)
	DeviceManager.scan_pci_bus ();
	(*begin try
		let dev = List.find (fun d -> d.vendor = 0x10EC && d.device = 0x8139) pci_devices in
		let x = RealTek8139.create dev in
		Vt100.printf "Created realtek 8139 device\r\n";
		let client = DhcpClient.create x in
		DhcpClient.register client;
	with Not_found ->
		Vt100.printf "No realtek 8139 found\r\n";
	end;*)
	(*let sample = [| 0;255;0;255;0;255;0;128;255;127;0;63;127;190;255;190;127;63;0 |] in
	begin try
		Sb16.output (Array.concat (Array.to_list (Array.make 256 sample)));
	with Sb16.Timeout -> Vt100.printf "sb error\n" end;*)
	ignore (Thread.create echo_shell ()) (* start the echo shell *)
	; Vt100.printf "Starting kernel link\r\n"
	(*; ELF.LinkKernel.link ()*)
	; ignore (ElfParsing.parse "foo" Bitstring.empty_bitstring)
