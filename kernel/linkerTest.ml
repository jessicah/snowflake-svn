
(* Build the snowflake kernel (eventually) *)

(* -T kernel/kernel.ldscript -L . -L libraries/stdlib -L libraries/threads -L libraries/extlib -L libraries/bitstring *)

let input_files = [|
		"kernel/snowflake.native.startup.o";
		"kernel/snowflake.o";
		"kernel/vga.o";
		"kernel/tarFile.o";
		"kernel/trie.o";
		"kernel/realTek8139.o";
		"kernel/networkStack.o";
		"kernel/keyboard.o";
		"kernel/interrupts.o";
		"kernel/dhcpClient.o";
		"kernel/networkProtocolStack.o";
		"kernel/PCI.o";
		"kernel/ELF.o";
		"kernel/vt100.o";
		"libraries/bigarray/bigarray.o";
		"libraries/bitstring/bitstring.a";
		"libraries/extlib/extlib.a";
		"libraries/threads/threads.a";
		"libraries/stdlib/stdlib.a";
		"libraries/kernel/stage1.o";
		"libraries/kernel/stage2.o";
		"libkernel.a";
		"libm.a";
		"libc.a";
		"libgcc.a";
		"libbigarray.a";
		"libthreads.a";
		"libbitstring.a";
		(*"libkernel.a";
		"libgcc.a";
		"libc.a";
		"libm.a";
		"libbigarray.a";
		"libthreads.a";
		"libbitstring.a";*)
	|]
