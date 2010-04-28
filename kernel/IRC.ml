
(* Snowflake's text-mode IRC client *)

open IrcTypes

open ExtString

(* freenode: 216.165.191.52, or a list of many, many others... :P *)
let freenode = NetworkProtocolStack.IPv4.Addr (216, 155, 130, 130)

module BufferedIO = struct
end

let clean_nick user =
	try
		String.sub user 0 (String.index user '!')
	with Not_found -> user

let run server port nick pass channel =
	Vt100.printf "Connecting to server...";
	(*let writer, read_line = TCP.open_channel server port in*)
	let socket = TCP.open_readline server port in
	let writer, read_line = socket.TCP.send, socket.TCP.recv in
	Vt100.printf "done!\n";
	let sender sender = try
			fst (String.split sender "!")
		with _ -> sender
	in
	
	let writef args =
		Printf.kprintf writer (args ^^ "\r\n")
	in
	
	(* send IRC initiation stuff *)
	writef "PASS :%s %s" nick pass;
	writef "USER %s %s@snowflake-os.googlecode.com %s :%s" nick nick
		(NetworkProtocolStack.IPv4.to_string server)
		"Snowflake IRC Client";
	writef "NICK %s" nick;
	
	Vt100.printf "Sent user registration, waiting for response...\n";
	
	let issued_quit = ref false in
	
	ignore (Thread.create begin fun () ->
		while not !issued_quit do
			(* shell does line buffering for us :) *)
			let line = Shell.read_line Shell.input in
			(* Let's make Snowflake a better IRC client! *)
			if String.length line >= 4 then begin
			match String.sub line 0 3 with
				  "/me" ->	(* CTCP action *)
					writef "PRIVMSG %s :\001ACTION %s \001" channel (String.slice ~first:4 line)
				| "/n " ->	(* Change nickname *)
					writef "NICK %s" (String.slice ~first:3 line)
				| "/j " -> 	(* Join channel *)
					writef "JOIN %s" (String.slice ~first:3 line)
				| "/p " ->	(* Part channel *)
					writef "PART %s" (String.slice ~first:3 line)
				| "/m " ->	(* Channel mode *)
					writef "MODE %s" (String.slice ~first:3 line)
				| "/um" ->	(* User mode *)
					(* need the current nickname which might have changed
						writef ":MODE %s %s" nick (String.slice ~first:4 line)
					*)
					Vt100.printf "Not implemented"
				| "/q " ->	(* Quit IRC *)
					writef "QUIT :%s" (String.slice ~first:3 line);
					TCP.close socket;
					issued_quit := true;
					Thread.yield ();
				| "/? " ->	(* Help! *)
					Vt100.printf "/me for CTCP action, /n for nickname, /j for join, /p for part (type /p #chan :REASON if you want a reason), /m for channel modes, /um for user modes, /q for quit, /? for this help"
				| _ ->	(* Anything else is a message. *)
					writef "PRIVMSG %s :%s" channel line;
			end else writef "PRIVMSG %s :%s" channel line;
		done;
	end () "irc read loop");
	
	while not !issued_quit do
		let line = read_line () in
		try match IrcParser.args IrcLexer.message (Lexing.from_string line) with
			(* Numeric commands *)
			| _, Numeric 001, _ -> writef "JOIN %s" channel; Vt100.printf "Joining channel %s\n" channel
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
			| Some s, Privmsg, target :: msg :: []
				when String.starts_with msg "\001PING " ->
					(* CTCP Ping *)
					writef "NOTICE %s :%s" (clean_nick s) msg
			| Some s, Privmsg, target :: msg :: []
				when String.starts_with msg "\001VERSION" ->
					(* CTCP Version *)
					writef "NOTICE %s :\001VERSION SnowflakeCLI SnowflakeOS-svn-i86 snowflake-os.googlecode.com\001" (clean_nick s)
			| Some s, Privmsg, target :: msg :: [] ->
					(* Message *)
					Vt100.printf "(%s) <%s> %s\n" target (sender s) msg
			
			(* Misc. commands *)
			| None, Ping, data :: [] ->
					writef "PONG :%s" data
			| _ -> ()
		with
			| End_of_file ->
				issued_quit := true (* just in case other end terminated it *)
			| ex ->
				Vt100.printf "Error: %s (%s)\n" (Printexc.to_string ex) line
	done

let nick = ref "snowflake-os"
let pass = ref ""

let test () =
	if !nick = "snowflake-os" && !pass = "" then
		Vt100.printf "irc: specify password for snowflake-os or use a different nick\n"
	else run freenode 8000 !nick !pass "#AWOS"

open Shell
open Arg

let init () =
	add_command "irc" test [
		"-nick", Set_string nick, " <nickname>";
		"-pass", Set_string pass, " <password";
	]
