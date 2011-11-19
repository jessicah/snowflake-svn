
open Bigarray

let () =
	Random.self_init (); (* seed the random number generator *)
	Debug.init (); (* replace stderr with one that writes to serial port *)
	Ovt100.init (); (* replace stdout with one that writes to whatever the current console is *)
	
	(* turn on backtrace stuff; need to fix threading for this... *)
	Printexc.record_backtrace true;

	(* initialise a bunch of devices *)
	Printf.eprintf "Random, Debug, Ovt100 initialised\n";
	Vga.init ();
	Printf.eprintf "Vga initialised\n";
	(*GraphicsConsole.init ();
	Printf.eprintf "GraphicsConsole initialised\n";*)
	Keyboard.init ();
	Printf.eprintf "Keyboard initialised\n";
	Ac97.init ();
	Printf.eprintf "Ac97 initialised\n";
	Pcnet.init ();
	Printf.eprintf "Pcnet initialised\n";
	RealTek8139.init ();
	Printf.eprintf "RealTek8139 initialised\n";
	NetworkStack.init ();
	Printf.eprintf "NetworkStack initialised\n";
	IRC.init ();
	Printf.eprintf "IRC initialised\n";
	IDE.init ();
	Printf.eprintf "IDE initialised\n";
	Tar_vfs.init ();
	Printf.eprintf "Tar_vfs initialised\n";
	Files.init ();
	Printf.eprintf "Files initialised\n";
	
	(*ICH0.init ();
	IDE.init ();
	RealTek8139.init ();
	E1000.init ();
	NetworkStack.init ();
	(*MusicPlayer.init ();
	TarFileSystem.init ();*)
	IRC.init ();
	StreamingPlayer.init ();*)

	(*Dynlink.loadfile "/tarfs/plugin.cmxs";*)
	
	(* let interrupts run *)
	Asm.sti ();
	
	(* probe the PCI bus and load any drivers it can find *)
	DeviceManager.scan_pci_bus ();
	
	(*(* get the partitions for the primary master *)
	let partitions =
		begin try
			let disk = IDE.get IDE.Primary IDE.Master in
			let partitions = Partitions.partitions_t (IDE.read_disk disk) (fun _ _ _ -> ()) in
			(* display found partitions *)
			if partitions = [] then
				Vt100.printf "No partitions found on ide:0:0\r\n"
			else begin
				Vt100.printf "Partitions on ide:0:0:\r\n";
				List.iter (fun p ->
					let p = p.Partitions.info in
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
	let rec loop = function
		| [] -> ()
		| x :: xs ->
			begin try
				let fs = Ext2fs.create x in
				(* add the FS to something... *)
				Vt100.printf "added a file system\n";
				FileSystems.set_fs fs;
				FileSystems.init ();
				Play.init ();
			with Not_found -> loop xs
			end
	in loop partitions;
	(*List.iter Ext2fs.init partitions;*)*)
	
	(*begin try
		let entries = Vfs.read_dir ["tarfs"] in
		Vt100.printf "Directory listing for /tarfs:\n";
		List.iter begin fun entry ->
				Vt100.printf "\t%s\n" entry
			end entries;
	with exn ->
		Vt100.printf "Failure reading directory listing for /tarfs: %s\n" (Printexc.to_string exn)
	end*)
	
	(* finished, so launch the shell *)
	Printf.eprintf "Launching Shell... ";
	Shell.init ();
	Printf.eprintf "completed; snowflake init exiting.\n"
