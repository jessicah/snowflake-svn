
let m = Mutex.create ()

let thread_1 () =
	while true do
		Mutex.lock m;
		Vt100.printf "thread 1\n";
		Mutex.unlock m;
		Thread.yield();
	done

let thread_2 () =
	while true do
		Mutex.lock m;
		Vt100.printf "thread 2\n";
		Mutex.unlock m;
		Thread.yield();
	done

let () =
    Vga.init (); (* set up a pretty console font *)
	let t1 = Thread.create thread_1 () in
	let t2 = Thread.create thread_2 () in
    Vt100.printf "Hello, from ML :)\nUsing ocaml version: %s\n" Sys.ocaml_version;
	Asm.sti ();
