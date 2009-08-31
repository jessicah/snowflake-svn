
let () =
	(* seed the random number generator *)
	Random.self_init ();
	
	(* initialise a bunch of devices *)
	Vga.init ();
	Keyboard.init ();
	ICH0.init ();
	IDE.init ();
	RealTek8139.init ();
	E1000.init ();
	(*NetworkStack.init ();
	MusicPlayer.init ();
	TarFileSystem.init ();*)
	IRC.init ();
	
	(* let interrupts run *)
	Asm.sti ();
	
	(* probe the PCI bus and load any drivers it can find *)
	DeviceManager.scan_pci_bus ();
	
	(* get the partitions for the primary master *)
	let partitions =
		begin try
			let disk = IDE.get IDE.Primary IDE.Master in
			let partitions = Partitions.partitions (IDE.read_disk disk) in
			(* display found partitions *)
			if partitions = [] then
				Vt100.printf "No partitions found on ide:0:0\r\n"
			else begin
				Vt100.printf "Partitions on ide:0:0:\r\n";
				List.iter (fun p ->
					Vt100.printf "type: %s, start: %d, length: %d\r\n"
						(Partitions.code_to_string p.Partitions.code)
						p.Partitions.start
						p.Partitions.length) partitions
			end;
			partitions
		with
			| Not_found ->
				Vt100.printf "Device ide:0:0 not found\r\n";
				[]
			| _ ->
				Vt100.printf "Error accessing ide:0:0...\r\n";
				[]
		end
	in
	
	(* finished, so launch the shell *)
	Shell.init ()
