
let echo_shell () =
	while true do
		Vt100.printf "%c" (Keyboard.get_char ())
	done

(*let print_ip () = function IPv4.Addr (x1,x2,x3,x4) ->
	Printf.sprintf "%d.%d.%d.%d" x1 x2 x3 x4

let print_dhcp packet =
	(*Vt100.printf "Transaction ID: %ld\r\n" packet.DHCP.transaction;*)
	Vt100.printf "Client IP: %a\r\n" print_ip packet.DHCP.client;
	Vt100.printf "Server IP: %a\r\n" print_ip packet.DHCP.server;
	(*Vt100.printf "Options: %d present\r\n" (List.length packet.DHCP.options);
	begin try 
		let z = List.find (fun o -> (fst o) = 0x35) packet.DHCP.options in
		Vt100.printf "Message: %d\r\n" (List.hd (snd z))
	with Not_found -> () end*) ()*)

let print_device dev =
	Vt100.printf "PCI: %04X:%04X\r\n" dev.PCI.vendor dev.PCI.device

open PCI

let () =
    Vga.init (); (* set up a pretty console font *)
	Keyboard.init (); (* set up the keyboard handler *)
	Vt100.printf "Hello, from ML :)\nUsing ocaml version: %s\n" Sys.ocaml_version;
	Asm.sti ();
	let pci_devices = PCI.probe_bus () in
	(*List.iter print_device pci_devices;*)
	begin try
		let dev = List.find (fun d -> d.vendor = 0x10EC && d.device = 0x8139) pci_devices in
		let x = RealTek8139.create dev in
		Vt100.printf "Created realtek 8139 device\r\n";
		let client = DhcpClient.create x in
		DhcpClient.register client;
	with Not_found ->
		Vt100.printf "No realtek 8139 found\r\n";
	end;
	(* test ELF header parsing... *)
	let data = Multiboot.open_module () in
	let str = String.create (Bigarray.Array1.dim data) in
	for i = 0 to String.length str - 1 do
		str.[i] <- data.{i}
	done;
	let tarfile = TarFile.open_tar_file str in
	(*let filelist = TarFile.dir_list tarfile "_build/kernel" in
	List.iter (Vt100.printf "Path: %s\n") filelist;
	List.iter begin fun filename ->
		let filename = "_build/kernel/" ^ filename in
		try
		ELF.print_header
			(ELF.parse_elf_header (Bitstring.bitstring_of_string
				(TarFile.read_file tarfile filename)))
		with exc -> Vt100.printf "Error: %s (%s)\n" (Printexc.to_string exc) filename
	end filelist;*)
	Array.iter begin fun filename ->
		try
			ELF.print (ELF.parse filename (Bitstring.bitstring_of_string (TarFile.read_file tarfile filename)))
		with exc ->
			Vt100.printf "Error: %s (%s)\n" (Printexc.to_string exc) filename
	end LinkerTest.input_files;
	ignore (Thread.create echo_shell ()) (* start the echo shell *)
