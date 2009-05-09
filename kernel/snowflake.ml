
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
    E1000.init ();
	IDE.init ();
	ICH0.init ();
	RealTek8139.init ();
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
	with Sb16.Timeout -> () end;*)
	ignore (Thread.create echo_shell ());
	begin try
		Vt100.printf "Attempting to load and play wave file...\n";
		let wave_data = Multiboot.open_module () in
		let wave = AudioMixer.Wave.read (BlockIO.make wave_data) in
		AudioMixer.play wave;
	with ex ->
		Vt100.printf "audio mixer: %s\n"
			(Printexc.to_string ex)
	end;
	begin try
		Vt100.printf "Attempting to send raw packet...\n";
		let tftp_packet = "\x00\x0f\xfe\x68\xb1\xef\x00\x30\x4f\x04\x58\x0b\x08\x00\x45\x00\x00\x4a\x00\x02\x00\x00\x40\x11\x6d\xed\x82\x7b\x83\xd9\x82\x7b\x83\xe4\x04\x00\x00\x45\x00\x36\x88\x24\x00\x01\x73\x6e\x6f\x77\x66\x6c\x61\x6b\x65\x2e\x6e\x61\x74\x69\x76\x65\x00\x6f\x63\x74\x65\x74\x00\x62\x6c\x6b\x73\x69\x7a\x65\x00\x31\x34\x33\x32\x00\x74\x73\x69\x7a\x65\x00\x30\x00" in
		NetworkStack.send tftp_packet;
		let reply = NetworkStack.recv () in
		Vt100.printf "Got a reply...\n%s\n" reply
	with ex ->
		Vt100.printf "netstack: %s\n" (Printexc.to_string ex)
	end
	