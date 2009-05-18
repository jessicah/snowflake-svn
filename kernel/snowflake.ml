
let echo_shell () =
	while true do
		Vt100.printf "%c" (Keyboard.get_char ())
	done

let () =
	(* seed the random number generator *)
	Random.self_init ();
	
	(* initialise a bunch of devices *)
	Vga.init ();
	Keyboard.init ();
	ICH0.init ();
	IDE.init ();
	
	(* let interrupts run *)
	Asm.sti ();
	
	(* probe the PCI bus and load any drivers it can find*)
	DeviceManager.scan_pci_bus ();
	
	(* simple thread to echo characters typed *)
	ignore (Thread.create echo_shell () "echo shell");
	
	(* finished *)
	()
