
(* Build the snowflake kernel (eventually) *)

(* -T kernel/kernel.ldscript -L . -L libraries/stdlib -L libraries/threads -L libraries/extlib -L libraries/bitstring *)

let input_files = [|
		"kernel/snowflake.native.startup.o";
		"kernel/snowflake.o";
		"kernel/vga.o";
		"kernel/tarFile.o";
		"kernel/trie.o";
		"kernel/keyboard.o";
		"kernel/interrupts.o";
		"kernel/PCI.o";
		"kernel/deviceManager.o";
		"kernel/ELF.o";
		"kernel/vt100.o";
		"kernel/linkerTest.o";
		"kernel/audioMixer.o";
		"kernel/blockIO.o";
		"kernel/e1000.o";
		"kernel/ext2fs.o";
		"kernel/fileSystems.o";
		"kernel/ICH0.o";
		"kernel/ircLexer.o";
		"kernel/IRC.o";
		"kernel/ircParser.o";
		"kernel/kernelBuffer.o";
		"kernel/networkProtocolStack.o";
		"kernel/networkStack.o";
		"kernel/packetParsing.o";
		"kernel/partitions.o";
		"kernel/play.o";
		"kernel/realTek8139.o";
		"kernel/ringBuffer.o";
		"kernel/shell.o";
		"kernel/IDE.o";
		"kernel/TCP.o";
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
