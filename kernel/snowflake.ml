
let m = Mutex.create ()

let thread_1 () =
	while true do
		Mutex.lock m;
		Vt100.printf "@";
		Mutex.unlock m;
	done

let thread_2 () =
	while true do
		Mutex.lock m;
		Vt100.printf "0";
		Mutex.unlock m;
	done

let () =
    Vga.init (); (* set up a pretty console font *)
	let t1 = Thread.create thread_1 () in
	let t2 = Thread.create thread_2 () in
    Vt100.printf "Hello, from ML :)\nUsing ocaml version: %s\n" Sys.ocaml_version;
	Asm.sti ();
