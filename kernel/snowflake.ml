
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
	ICH0.init ();
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
		let get_data =
			let i = ref 0 in
			begin fun () ->
				let ch = wave_data.{!i} in
				incr i;
				ch
			end
		in
		let chan = IO.from_in_chars (object
			method get () = get_data ()
			method close_in () = ()
		end) in
		let wave = AudioMixer.Wave.read chan in
		AudioMixer.play wave;
	with ex ->
		Vt100.printf "audio mixer: %s\n"
			(Printexc.to_string ex)
	end
