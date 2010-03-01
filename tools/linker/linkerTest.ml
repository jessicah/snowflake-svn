
(* Build the snowflake kernel (eventually) *)

(* -T kernel/kernel.ldscript -L . -L libraries/stdlib -L libraries/threads -L libraries/extlib -L libraries/bitstring *)

let input_files = [
		"../../_build/kernel/snowflake.native.startup.o";
		"../../_build/kernel/snowflake.o";
		(*"kernel/vga.o";*)
		(*"../../_build/kernel/tarFile.o";
		"../../_build/kernel/trie.o";*)
		(*"kernel/keyboard.o";*)
		(*"../../_build/kernel/interrupts.o";*)
		(*"kernel/PCI.o";
		"kernel/deviceManager.o";*)
		"../../_build/kernel/vt100.o";
		(*"kernel/audioMixer.o";
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
		"kernel/TCP.o";*)
		"../../_build/libraries/bigarray/bigarray.o";
		"../../_build/libraries/bitstring/bitstring.a";
		"../../_build/libraries/extlib/extlib.a";
		"../../_build/libraries/threads/threads.a";
		"../../_build/libraries/stdlib/stdlib.a";
		"../../_build/libraries/kernel/stage1.o";
		"../../_build/libraries/kernel/stage2.o";
		"../../_build/libkernel.a";
		"../../_build/libm.a";
		"../../_build/libc.a";
		"../../_build/libgcc.a";
		"../../_build/libbigarray.a";
		"../../_build/libthreads.a";
		"../../_build/libbitstring.a";
		(*"libkernel.a";
		"libgcc.a";
		"libc.a";
		"libm.a";
		"libbigarray.a";
		"libthreads.a";
		"libbitstring.a";*)
	]
