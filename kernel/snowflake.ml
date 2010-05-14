
external set_vbe_mode : int -> int32 = "snowflake_vbe_switch"

let () =
	(* seed the random number generator *)
	Random.self_init ();
	
	(* initialise a bunch of devices *)
	Vga.init ();
	Keyboard.init ();
	(*ICH0.init ();
	IDE.init ();
	RealTek8139.init ();
	E1000.init ();
	NetworkStack.init ();
	(*MusicPlayer.init ();
	TarFileSystem.init ();*)
	IRC.init ();
	StreamingPlayer.init ();*)
	
	(* let interrupts run *)
	Asm.sti ();
	
	(*(* switch to gfx mode *)
	Debug.printf "Switching to gfx mode...";
	(* 0x144 requires -vga vmware *)
	let frame_buffer = set_vbe_mode 0x144 in
	Debug.printf " completed! \o/\n";
	
	let surface = Cairo_bigarray.of_bigarr_32 ~alpha:false (Asm.matrix32 frame_buffer 768 1024) in
	let cr = Cairo.create surface in
	
	(*Fdclock.FDHand.draw_now cr 200. 200. true;*)
	
	let fb = Asm.matrix32 frame_buffer 768 1024 in
	
	let f = Fonts.BDF.get "Courier" Fonts.BDF.Normal Fonts.BDF.Regular 14 in
	(*Bigarray.Array2.fill fb 0xffffff;*)
	Fonts.draw_text fb "snowflake-os: graphics mode!"
		f (50,50) (255,201,14);
	Fonts.draw_text fb "red text"
		f (50,100) (255,0,0);
	Fonts.draw_text fb "green text"
		f (50,150) (0,255,0);
	Fonts.draw_text fb "blue text"
		f (50,200) (0,0,255);
	*)
	GraphicsConsole.init ();
	
	let face, family, style = Freetype.face Gothic.data in
	
	Debug.printf "Loaded font: %s (%s)\n" family style;
	
	while true do
		GraphicsConsole.put (Keyboard.get_char ());
	done;
	
	(*Fdclock.FDHand.draw_now cr 200. 200. true;*)
	
	(*(* probe the PCI bus and load any drivers it can find *)
	DeviceManager.scan_pci_bus ();
	
	(* get the partitions for the primary master *)
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
	
	(* finished, so launch the shell *)
	Shell.init ()
