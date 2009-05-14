
open Bigarray

let echo_shell () =
	while true do
		Vt100.printf "%c" (Keyboard.get_char ())
	done

let print_device dev =
	Vt100.printf "PCI: %04X:%04X\r\n" dev.PCI.vendor dev.PCI.device

open PCI

let () =
	(* seed the random number generator *)
	Random.self_init ();
	Vga.init (); (* set up a pretty console font *)
	Keyboard.init (); (* set up the keyboard handler *)
	(*Tulip.init (); (* unfortunately it won't get linked in otherwise... *)
    E1000.init ();
	IDE.init ();*)
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
	ignore (Thread.create echo_shell () "echo shell");
	(*begin try
		Vt100.printf "Attempting to load and play wave file...\n";
		let wave_data = Multiboot.open_module () in
		let wave = AudioMixer.Wave.read (BlockIO.make wave_data) in
		ignore (Thread.create AudioMixer.play wave "audio mixer");
	with ex ->
		Vt100.printf "audio mixer: %s\n"
			(Printexc.to_string ex)
	end;*)
	begin try
		NetworkStack.init ();
		Vt100.printf "Reading iTunes server info at 130.123.131.228...\n";
		let server = [130;123;131;228] in
		Vt100.printf "server-info...\n";
		let server_info = HTTP.request "/server-info" [] server 3689 in
		(*DAAP.parse_server_info server_info;*)
		Vt100.printf "content-codes...\n";
		let content_codes = HTTP.request "/content-codes" [] server 3689 in
		(* ignore *)
		Vt100.printf "login... session-id = ";
		let login = HTTP.request "/login" [] server 3689 in
		(* got login response *)
		let session_id = DAAP.parse_login login in
		Vt100.printf "%ld\n" session_id;
		(* get revision id *)
		Vt100.printf "update... revision-id = ";
		let update = HTTP.request (Printf.sprintf "/update?session-id=%ld" session_id) [] server 3689 in
		let revision_id = DAAP.parse_update update in
		Vt100.printf "%ld\n" revision_id;
		(*(* get database list *)
		Vt100.printf "databases...\n";
		let database_list = HTTP.request (Printf.sprintf "/databases?session-id=%ld&revision-id=%ld" session_id revision_id) [] server 3689 in
		(*DAAP.output_databases database_list;*)
		(* get song list *)
		Vt100.printf "songs...\n";
		let r = Printf.sprintf "/databases/%ld/items?type=music&session-id=%ld&revision-id=%ld" Int32.one session_id revision_id in
		let song_list = HTTP.request r [] server 3689 in
		(*DAAP.output_songs song_list;*)*)
		Vt100.printf "getting wave data...\n";
		let r = Printf.sprintf
			"/databases/1/items/1829.wav?session-id=%ld" session_id in
		let wave_data = HTTP.open_stream r server 3689 in
		Vt100.printf "got input stream to the wave data!\n";
		let input_buffer = BlockIO.make
			(Array1.create int8_unsigned c_layout 131072)
		in
		(* process first buffer, which _should_ include the wave header... *)
		Array1.blit_from_string (IO.nread wave_data 131072) input_buffer.BlockIO.data;
		Vt100.printf "play it...\n";
		AudioMixer.play (AudioMixer.Wave.read input_buffer);
		(* now process all remaining buffers *)
		while true do
			Vt100.printf ".";
			input_buffer.BlockIO.pos <- 0; (* reset position *)
			Array1.blit_from_string (IO.nread wave_data 131072) input_buffer.BlockIO.data;
			AudioMixer.play_raw input_buffer;
		done;
		Vt100.printf "[end of wave file]\n";
	with
		|Invalid_argument x ->
			Vt100.printf "daap test: invalid argument: %s\n" x
		| ex ->
			Vt100.printf "daap test: %s\n" (Printexc.to_string ex)
	end
