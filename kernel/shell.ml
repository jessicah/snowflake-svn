
(* a lil command interpreter -- not really a shell *)

let commands = Hashtbl.create 7

let add_command name f specs =
	Hashtbl.add commands name (f, Arg.align specs)

let no_anon arg =
	raise (Arg.Bad (Printf.sprintf "unknown argument (%s)" arg))

let read_line ic =
	let line = IO.read_line ic in
	let length = String.length line in
	let stack = Stack.create () in
	for i = 0 to length - 1 do
		if line.[i] = '\b' then begin
			if not (Stack.is_empty stack) then
				ignore (Stack.pop stack)
		end else
			Stack.push line.[i] stack
	done;
	let s_length = Stack.length stack in
	if s_length = length then
		(* we didn't remove anything *)
		line
	else begin
		let s = String.create s_length in
		for i = s_length - 1 downto 0 do
			s.[i] <- Stack.pop stack
		done;
		s
	end

let rec replace_all s sub rep = match ExtString.String.replace s sub rep with
	| true, s -> replace_all s sub rep
	| false, s -> s

let split line =
	let line = replace_all line "\\ " "%20" in
	let parts = ExtString.String.nsplit line " " in
	List.map begin fun part ->
			replace_all part "%20" " "
		end parts

let input = IO.from_in_chars(object
		method get () =
			let ch = Keyboard.get_char () in
			Vt100.printf "%c" ch;
			ch
		method close_in () = ()
	end)

let shell () =
	Vt100.printf "Welcome to the Snowflake shell\n\n";
	while true do
		Vt100.printf "> ";
		let line = read_line input in
		let parts = split line in
		let current = ref 0 in
		match parts with
		| [] -> ()
		| x :: rest ->
			begin try
				let f, spec_list = Hashtbl.find commands x in
				begin try
					Arg.parse_argv ~current (Array.of_list parts) spec_list no_anon "";
					f ()
				with Arg.Help msg | Arg.Bad msg ->
					Vt100.printf "%s" msg
				end
			with
			| Not_found ->
				Vt100.printf "command not found: %s\n" x
			| Failure msg ->
				Vt100.printf "%s: %s\n" x msg
			| ex ->
				Vt100.printf "%s: unhandled error\n%s\n"
					x (Printexc.to_string ex)
			end
	done

(* like always, an init function called from snowflake.ml *)

let init () =
	(* add a lil help command *)
	add_command "help" begin fun () ->
			Vt100.printf "Available commands:\n";
			Hashtbl.iter (fun name _ ->
				Vt100.printf " %s\n" name
			) commands
		end [];
	(* then spawn the shell *)
	ignore (Thread.create shell () "shell")
