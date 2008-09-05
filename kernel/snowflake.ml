
open Pervasives
open Bigarray
open Asm

external print_endline : string -> unit = "snowflake_print_endline"

let count = ref 0

let thread1 () =
	while true do
		incr count;
		print_endline (string_of_int !count);
		(*(* same as below :( *)
		Thread.yield ();*)
	done

let thread2 () =
	while true do
		print_endline "x";
		(*(* need to yield, as no scheduling opportunities in this loop *)
		Thread.yield ();*)
	done

let () =
	let s = "Hello, from ML :) Using ocaml version " ^ Sys.ocaml_version in
	let video = matrix16 0xB8000l 80 25 in
	Array2.fill video 0x0720; (* ' ' : 0x07 *)
	print_endline s;
	ignore (Thread.create thread1 ());
	ignore (Thread.create thread2 ());
	Asm.sti ()
