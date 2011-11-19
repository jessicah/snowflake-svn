
open Shell
open Arg

let init () =
	let got_file = ref false in
	add_command "cat" (fun () -> if not !got_file then print_endline "meow!"; got_file := false)
		~anon:(fun filename ->
			got_file := true;
			try
				let ic = open_in filename in
				while true do print_char (input_char ic) done
			with 
				| Not_found -> print_endline "cat: file not found or is directory"
				| End_of_file -> ())
		[]
