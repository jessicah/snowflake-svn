
(* a lil command interpreter -- not really a shell *)

let commands = Hashtbl.create 7

let no_anon arg =
	raise (Arg.Bad (Printf.sprintf "unknown argument (%s)" arg))

let add_command name f ?(anon = no_anon) specs =
	Hashtbl.add commands name (f, Arg.align specs, anon)

let input2 = IO.from_in_chars(object (self)
		val mutable len = 0
		method get () =
			let ch = Keyboard.get_char () in
			if ch = '\b' && len = 0 then
				self#get ()
			else begin
				begin match ch with
					| '\b' -> len <- len - 1
					| '\n' -> len <- 0
					| _    -> len <- len + 1
				end;
				if ch <> '\n' then Vt100.printf "%c" ch;
				ch
			end
		method close_in () = ()
	end)

let read_input () =
	let stack = Stack.create () in
	let rec loop () =
		match IO.read input2 with
			| '\n' ->
					let len = Stack.length stack in
					let line = String.create len in
					for i = len - 1 downto 0 do
						line.[i] <- Stack.pop stack
					done;
					line
			| '\b' ->
					ignore (Stack.pop stack);
					loop ()
			| ch ->
					Stack.push ch stack;
					loop ()
	in loop ()

(* will need to switch to using a parser soon... *)
let read_line () =
	let stack = Stack.create () in
	let rec loop in_quotes =
		if in_quotes then
			match Keyboard.get_char () with
			| '\b' ->
				ignore (Stack.pop stack);
				Vt100.printf "\b";
				loop (Stack.is_empty stack = false)
			| '"' ->
				(* closed the quoted string *)
				Vt100.printf "\"";
				loop false
			| ' ' ->
				Stack.push '\\' stack;
				Stack.push ' ' stack;
				Vt100.printf " ";
				loop true
			| ch ->
				Stack.push ch stack;
				Vt100.printf "%c" ch;
				loop true
		else
			match Keyboard.get_char () with
			| '\n' ->
				(* we have our line on the stack *)
				Vt100.printf "\n";
			| '\b' when Stack.is_empty stack ->
				(* nothing to delete; ignore it *)
				loop false
			| '\b' ->
				ignore (Stack.pop stack);
				Vt100.printf "\b";
				loop false
			| '"' ->
				Vt100.printf "\"";
				loop true
			| ch ->
				Stack.push ch stack;
				Vt100.printf "%c" ch;
				loop false
	in loop false;
	let len = Stack.length stack in
	let line = String.create len in
	for i = len - 1 downto 0 do
		line.[i] <- Stack.pop stack
	done;
	line

let rec replace_all s sub rep = match ExtString.String.replace s sub rep with
	| true, s -> replace_all s sub rep
	| false, s -> s

let split line =
	let line = replace_all line "\\ " "%20" in
	let parts = ExtString.String.nsplit line " " in
	List.map begin fun part ->
			replace_all part "%20" " "
		end parts

let input = IO.from_in_chars(object (self)
		val mutable len = 0
		method get () =
			let ch = Keyboard.get_char () in
			if ch = '\b' && len = 0 then
				self#get ()
			else begin
				begin match ch with
					| '\b' -> len <- len - 1
					| '\n' -> len <- 0
					| _    -> len <- len + 1
				end;
				Vt100.printf "%c" ch;
				ch
			end
		method close_in () = ()
	end)

let shell () =
	Vt100.printf "Welcome to the Snowflake shell\n\n";
	while true do
		Vt100.printf "> ";
		let line = read_line () in
		let parts = split line in
		let current = ref 0 in
		match parts with
		| [] -> ()
		| x :: rest ->
			begin try
				let f, spec_list, anon = Hashtbl.find commands x in
				begin try
					Arg.parse_argv ~current (Array.of_list parts) spec_list anon "";
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
