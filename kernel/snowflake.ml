
open Bigarray

external set_vbe_mode : int -> int32 = "snowflake_vbe_switch"

module Test = struct

	open Freetype
	
	type pen = {
		mutable x : int;
		mutable y : int;
	}
	
	let pen = { x = 0; y = 0 }
	
	let colour = ref 0xedca00

	let draw_glyph fb face glyph =
	
		let metrics = Internal.get_metrics face in
		
		if pen.x + glyph.advance_x / 64 > 1024 then begin
			pen.x <- 0;
			pen.y <- pen.y + (metrics.height lsr 4);
		end;
		
		let baseline = pen.y + (metrics.ascender lsr 4) in
		
		(*Debug.printf
			"rows: %d  width: %d  pitch: %d  mode: %s  bitleft: %d  bittop: %d  base: %d\n"
			glyph.bitmap.rows glyph.bitmap.width glyph.bitmap.pitch
			(match glyph.bitmap.pixel_mode with
				| PM_Mono -> "mono"
				| PM_Gray -> "gray"
				| _ -> "other")
			glyph.bitmap_left glyph.bitmap_top baseline;*)
		
		if glyph.bitmap.pixel_mode = PM_Mono then
			for row = 0 to glyph.bitmap.rows - 1 do
				for x = 0 to glyph.bitmap.width - 1 do
					if glyph.bitmap.buffer.{row, (x+1)/8} land (1 lsr (8-((x+1) mod 8))) <> 0 then
						fb.{baseline - glyph.bitmap_top + row, pen.x + glyph.bitmap_left + x} <- 0x00ff00l;
				done
			done
		else
			for row = 0 to glyph.bitmap.rows - 1 do
				for x = 0 to glyph.bitmap.width - 1 do
					if glyph.bitmap.buffer.{row, x} <> 0 then begin
						let alpha = glyph.bitmap.buffer.{row,x} in
						let alpha = (alpha lsl 16) lor (alpha lsl 8) lor alpha in
						let colour = Int32.of_int (!colour land alpha) in
						fb.{baseline - glyph.bitmap_top + row, pen.x + glyph.bitmap_left + x} <-
							(*(Int32.of_int (0xffffff - (glyph.bitmap.buffer.{row, x} * 0x010101)));*)
							colour;
					end
				done
			done;
		
		pen.x <- pen.x + glyph.advance_x / 64
	
	let draw_text fb face text =
		String.iter begin fun ch ->
			draw_glyph fb face (Internal.load_char face ch)
		end text

end

let () =
	(* seed the random number generator *)
	Random.self_init ();
	
	(* initialise a bunch of devices *)
	Vga.init ();
	Keyboard.init ();
	Ac97.init ();
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
	
	let face, family, style = Freetype.face Courier.data in
	
	Freetype.Internal.set_char_size face (12*64) 96;
	
	Debug.printf "Loaded font: %s (%s)\n" family style;
	
	let fb = GraphicsConsole.t.GraphicsConsole.frame_buffer in
		
	(*let plot bmp x y =
		for i = 0 to Array2.dim1 bmp.buffer - 1 do
			for j = 0 to Array2.dim2 bmp.buffer - 1 do
				fb.{x+i,y+j} <- Int32.of_int(bmp.buffer.{i,j} land 0x00FF00);
			done
		done
	in
	
	drawstring plot (100,100) face "hello snowflake-os (with freetype!)";*)
	
	(*Test.draw_text fb face "hello snowflake-os (with freetype!) using courier new :-)";
	
	while true do
		Test.draw_text fb face (String.make 1 (Keyboard.get_char ()));
	done;*)
	UTF8.iter GraphicsConsole.uput "雪片へようこそ (Welcome to Snowflake)\000";
		
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
