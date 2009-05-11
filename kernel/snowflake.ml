
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
		Vt100.printf "Starting netstack...\n";
		NetworkStack.init ();
		(*Vt100.printf "Opening connection to 130.123.131.228:3689...\n";
		let (ic,oc) = NetworkStack.API.open_tcp (NetworkProtocolStack.IPv4.Addr (130, 123, 131, 228)) 3689 in
		IO.printf oc "GET /server-info HTTP/1.1\r\n\r\n";
		let response = IO.nread ic 80 in
		Vt100.printf "response:\n%s\n[end]\n" response;*)
		let f = TCP.connect (NetworkProtocolStack.IPv4.Addr (130, 123, 131, 228)) 3689 in
		f "GET /server-info HTTP/1.1\r\n\r\n";
		(* : now it's not working... =/
		while true do
			if Queue.is_empty q then begin
				Thread.yield ();
			end else begin
				Vt100.printf "response:\n%s\n[end]\n" (Queue.take q);
			end;
			Thread.yield ();
		done*)
	with ex ->
		Vt100.printf "netstack: %s\n" (Printexc.to_string ex)
	end
