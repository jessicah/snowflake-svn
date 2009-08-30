
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
	
	(* finished, so launch the shell *)
	Shell.init ()
