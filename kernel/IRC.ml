
(* Snowflake's text-mode IRC client *)

open IrcTypes

open ExtString

module UI = struct
	
	(* handles rendering/state of the IRC client interface *)
	
	let title = [
			"                           __ _       _          _          ";
			" ___ _ __       __      __/ _| | __ _| | _____ _(_)_ __ ___ ";
			"/ __| '_ \\  _/\\_\\ \\ /\\ / / |_| |/ _` | |/ / _ (_) | '__/ __|";
			"\\__ \\ | | |\\    /\\ V  V /|  _| | (_| |   <  __/_| | | | (__ ";
			"|___/_| |_|/_  _\\ \\_/\\_/ |_| |_|\\__,_|_|\\_\\___(_)_|_|  \\___|";
			"             \\/                                             ";
		]
	
	type channel = {
		name : string;
		mutable lines : string list;
		mutable users : string list;
		mutable highlighted : bool;
		mutable activity : bool;
	}
	
	type state = {
		mutable channels : channel list;
		mutable active : channel;
	}
	
	let status_channel = {
			name = "[status]";
			lines = [
				(* 25 rows, 5 for text, 2 for headers, 18 empty *)
				""; ""; ""; ""; ""; ""; ""; ""; "";
				"\027[37;40;1m                                     __ _       _          _          ";
				"\027[37;40;1m           ___ _ __       __      __/ _| | __ _| | _____ _(_)_ __ ___ ";
				"\027[37;40;1m          / __| '_ \\  _/\\_\\ \\ /\\ / / |_| |/ _` | |/ / _ (_) | '__/ __|";
				"\027[37;40;1m          \\__ \\ | | |\\    /\\ V  V /|  _| | (_| |   <  __/_| | | | (__ ";
				"\027[37;40;1m          |___/_| |_|/_  _\\ \\_/\\_/ |_| |_|\\__,_|_|\\_\\___(_)_|_|  \\___|";
				"\027[37;40;1m                       \\/                                             ";
				""; ""; ""; ""; ""; ""; ""; "";
			];
			users = [];
			highlighted = false;
			activity = false;
		}
	
	let state = { channels = [status_channel]; active = status_channel }
	
	let empty_window = [""; ""; ""; ""; ""; ""; ""; ""; ""; ""; ""; ""; ""; ""; ""; ""; ""; ""; ""; ""; ""; ""; ""]
	
	let add_channel name =
		state.channels <- state.channels @ [ {
					name = name;
					lines = empty_window;
					highlighted = false;
					activity = false;
					users = [];
				}
			]
	
	let channel_name () =
		if state.active = status_channel then raise Not_found
		else state.active.name
	
	let highlight name =
		let chan = List.find begin fun ch -> ch.name = name end state.channels in
		chan.highlighted <- true
	
	let save, restore = "\027[s", "\027[u"
	
	let active_channel () =
		let chan = state.active in
		ignore (List.fold_left begin fun row text ->
				Vt100.printf "\027[s\027[%d;1f\027[0m\027[K%s\027[u" row text;
				row + 1
			end 2 chan.lines)
	
	let channel_status chan =
		if chan = state.active then
			Printf.sprintf "\027[7m  %s  \027[7m" chan.name
		else if chan.highlighted then
			Printf.sprintf "\027[41;37;1m  %s  \027[30;47;1m" chan.name
		else if chan.activity then
			Printf.sprintf "\027[42;37;1m  %s  \027[30;47;1m" chan.name
		else
			Printf.sprintf "  %s  " chan.name
	
	let status () =
		(* redraws the status line *)
		let channel_names = List.map channel_status  state.channels in
		(*let channel_names = [ "\027[7m  [status]  \027[7m"; "\027[42;37;1m  <#AWOS>  \027[30;47;1m"; "  <#XoMB>  " ] in*)
		let header = String.concat "" channel_names in
		Vt100.printf "%s\027[H\027[30;47;1m\027[K%s%s"
			save header restore
	
	let prompt () =
		Vt100.printf "\027[25;1f\027[37;44;1m\027[K> "
	
	let redraw () =
		status ();
		active_channel ();
		prompt ()
	
	let add_line ?(highlighted = false) ?(input = false) name line =
		let chan = List.find begin fun ch -> ch.name = name end state.channels in
		if highlighted then begin
			chan.lines <- List.tl chan.lines @ [
					Printf.sprintf "\027[33;1m%s\027[0m" line
				];
			chan.highlighted <- true && chan <> state.active;
		end else
			chan.lines <- List.tl chan.lines @ [line];
		(* we have activity on this channel *)
		if chan <> state.active && chan.highlighted = false then
			chan.activity <- true;
		if input then
			redraw ()
		else begin
			status ();
			active_channel ();
			Vt100.printf "\027[37;44;1m";
		end
	
	let cycle_channels () =
		let chan_after = List.fold_left begin fun (found,value) chan ->
				if not found && chan.name = state.active.name then
					(true, None)
				else if found && value = None then
					(true, Some chan)
				else
					(found, value)
			end (false, None) state.channels in
		begin match snd chan_after with
			| None -> state.active <- List.hd state.channels
			| Some chan -> state.active <- chan
		end;
		state.active.activity <- false;
		state.active.highlighted <- false;
		redraw ()
end

(* freenode: 216.165.191.52, or a list of many, many others... :P *)
let freenode = NetworkProtocolStack.IPv4.Addr (216, 155, 130, 130)

let clean_nick user =
	try
		String.sub user 0 (String.index user '!')
	with Not_found -> user

let status = "[status]"

let run server port nick pass channel' =
	(*UI.init ();*)
	UI.redraw ();
	UI.add_line status "Connecting to Freenode...";
	let socket = TCP.open_readline server port in
	let writer, read_line = socket.TCP.send, socket.TCP.recv in
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
	
	UI.add_line status "Sent user registration...";
	
	let issued_quit = ref false in
	
	let channel () = UI.channel_name () in
	
	ignore (Thread.create begin fun () ->
		while not !issued_quit do
			(* shell does line buffering for us :) *)
			let line = Shell.read_input () in
			(* Let's make Snowflake a better IRC client! *)
			begin try
			if String.length line >= 4 then begin
			match String.sub line 0 3 with
				  "/me" ->	(* CTCP action *)
					writef "PRIVMSG %s :\001ACTION %s \001" (channel ()) (String.slice ~first:4 line);
					UI.add_line ~input:true (channel ()) (Printf.sprintf " * %s %s" nick (String.slice ~first:4 line))
				| "/n " ->	(* Change nickname *)
					writef "NICK %s" (String.slice ~first:3 line)
				| "/j " -> 	(* Join channel *)
					UI.add_channel (String.slice ~first:3 line);
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
					Vt100.printf "\027[0m\027[J";
					Thread.yield ();
				| "/c " ->
					UI.cycle_channels ()
				| "/? " ->	(* Help! *)
					UI.add_line ~input:true status "Help!" (*"/me for CTCP action, /n for nickname, /j for join, /p for part (type /p #chan :REASON if you want a reason), /m for channel modes, /um for user modes, /q for quit, /? for this help"*)
				| _ ->	(* Anything else is a message. *)
					writef "PRIVMSG %s :%s" (channel ()) line;
					UI.add_line ~input:true (channel ()) (Printf.sprintf " <%s> %s" nick line)
			end else begin
				writef "PRIVMSG %s :%s" (channel ()) line;
				UI.add_line ~input:true (channel ()) (Printf.sprintf " <%s> %s" nick line)
			end
			with Not_found -> ()
			end;
		done;
	end () "irc read loop");
	
	while not !issued_quit do
		let line = read_line () in
		try match IrcParser.args IrcLexer.message (Lexing.from_string line) with
			(* Numeric commands *)
			| _, Numeric 001, _ ->
					writef "JOIN %s" channel';
					UI.add_channel channel';
					UI.add_line status (Printf.sprintf "Joining channel %s\n" channel')
			| _, Numeric _, _ -> ()
			
			(* Channel commands *)
			
			(* Messaging commands *)
			| Some s, Privmsg, target :: msg :: []
				when String.starts_with msg "\001ACTION " ->
					(* CTCP Action *)
					let msg = String.rchop msg in
					begin try
						let highlighted = String.exists msg nick in
						UI.add_line ~highlighted target (Printf.sprintf " * %s %s" (sender s) (String.slice ~first:8 msg));
					with Not_found ->
						()
					end
			| Some s, Privmsg, target :: msg :: []
				when String.starts_with msg "\001PING " ->
					(* CTCP Ping *)
					writef "NOTICE %s :%s" (clean_nick s) msg
			| Some s, Privmsg, target :: msg :: []
				when String.starts_with msg "\001VERSION" ->
					(* CTCP Version *)
					writef "NOTICE %s :\001VERSION snowflake:irc snowflake-os http://snowflake-os.googlecode.com\001" (clean_nick s)
			| Some s, Privmsg, target :: msg :: [] ->
					(* Message *)
					begin try
						let highlighted = String.exists msg nick in
						UI.add_line ~highlighted target (Printf.sprintf " <%s> %s" (sender s) msg);
					with Not_found ->
						()
					end
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
