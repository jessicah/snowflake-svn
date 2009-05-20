
(* Snowflake's text-mode IRC client *)

open IrcTypes

open ExtString

(* freenode: 216.165.191.52, or a list of many, many others... :P *)
let freenode = NetworkProtocolStack.IPv4.Addr (216, 165, 191, 52)

let run server port nick channel =
	Vt100.printf "Connecting to server...";
	let writer, reader = TCP.open_channel server port in
	Vt100.printf "done!\n";
	let sender sender = try
			fst (String.split sender "!")
		with _ -> sender
	in
	
	let writef args =
		Printf.kprintf writer (args ^^ "\r\n")
	in
	
	(* send IRC initiation stuff *)
	writef "PASS %s" "snowflake";
	writef "USER %s %s@snowflake-os.googlecode.com %s :%s" nick nick
		(NetworkProtocolStack.IPv4.to_string server)
		"Snowflake IRC Client";
	writef "NICK %s" nick;
	
	ignore (Thread.create begin fun () ->
		while true do
			(* shell does line buffering for us :) *)
			let line = Shell.read_line Shell.input in
			(* just send it to the channel *)
			(* only problem is that input and output will be interspersed... *)
			writef "PRIVMSG %s :%s" channel line;
		done;
	end () "irc read loop");
	
	while true do
		let line = IO.read_line reader in
		try match IrcParser.args IrcLexer.message (Lexing.from_string line) with
			(* Numeric commands *)
			| _, Numeric 001, _ -> writef "JOIN %s" channel
			| _, Numeric _, _ -> ()
			
			(* Channel commands *)
			
			(* Messaging commands *)
			| Some s, Privmsg, target :: msg :: []
				when String.starts_with msg "\001ACTION " ->
					(* CTCP Action *)
					let msg = String.rchop msg in
					Vt100.printf "(%s) * %s %s\n"
						target
						(sender s)
						(String.slice ~first:8 msg)
			| Some s, Privmsg, target :: msg :: [] ->
					(* Message *)
					Vt100.printf "(%s) <%s> %s\n" target (sender s) msg
			
			(* Misc. commands *)
			| None, Ping, data :: [] ->
					writef "PONG: %s" data
			| _ -> ()
		with ex ->
			Vt100.printf "Error: %s (%s)\n" (Printexc.to_string ex) line
	done

let test () = run freenode 8000 "snowflake-os" "#AWOS"

open Shell

let init () =
	add_command "irc" test []
