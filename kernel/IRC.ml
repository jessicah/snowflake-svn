
(* Snowflake's text-mode IRC client *)

open IrcTypes

open ExtString
open Printf (* change to Printf for vfs layer *)

module UI = struct
	
	(* handles rendering/state of the IRC client interface *)
	
	let title = [|
		"\027[37;40;1m                                     __ _       _          _          ";
		"\027[37;40;1m           ___ _ __       __      __/ _| | __ _| | _____ _(_)_ __ ___ ";
		"\027[37;40;1m          / __| '_ \\  _/\\_\\ \\ /\\ / / |_| |/ _` | |/ / _ (_) | '__/ __|";
		"\027[37;40;1m          \\__ \\ | | |\\    /\\ V  V /|  _| | (_| |   <  __/_| | | | (__ ";
		"\027[37;40;1m          |___/_| |_|/_  _\\ \\_/\\_/ |_| |_|\\__,_|_|\\_\\___(_)_|_|  \\___|";
		"\027[37;40;1m                       \\/                                             ";
	|]
	
	type channel = {
		name : string;
		mutable lines : string array;
		mutable users : string list;
		mutable highlighted : bool;
		mutable activity : bool;
	}
	
	type state = {
		mutable channels : channel list;
		mutable active : channel;
	}
	
	let dummy_channel = {
		name = "dummy channel";
		lines = [| |];
		users = [];
		highlighted = false;
		activity = false;
	}
	
	let mk_empty_window () = Array.make (snd (Ovt100.dims ()) - 2) ""
	
	let mk_status_channel () =
		let chan = {
			name = "[status]";
			lines = mk_empty_window ();
			users = [];
			highlighted = false;
			activity = false;
		} in
		for i = 0 to 5 do
			chan.lines.(i + (((snd (Ovt100.dims ()) - 6) / 2))) <- title.(i)
		done;
		chan
	
	let state = { channels = []; active = dummy_channel }
	
	let add_channel name =
		state.channels <- state.channels @ [ {
					name = name;
					lines = mk_empty_window ();
					highlighted = false;
					activity = false;
					users = [];
				}
			]
	
	let channel_name () =
		if state.active.name = "[status]" then raise Not_found
		else state.active.name
	
	let highlight name =
		let chan = List.find begin fun ch -> ch.name = name end state.channels in
		chan.highlighted <- true
	
	let save, restore = "\027[s", "\027[u"
	
	let active_channel () =
		let chan = state.active in
		Array.iteri (fun row line ->
			printf "\027[s\027[%d;1f\027[0m\027[K%s\027[u" (row+2) line)
			chan.lines
	
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
		let header = String.concat "" channel_names in
		printf "%s\027[H\027[30;47;1m\027[K%s%s"
			save header restore
	
	let prompt () =
		printf "\027[%d;1f\027[37;44;1m\027[K> " (snd (Ovt100.dims ()))
	
	let redraw () =
		let start = Asm.rdtsc () in
		status ();
		active_channel ();
		prompt ();
		let stop = Asm.rdtsc () in
		Printf.eprintf "redraw: %Ld\n" (Int64.sub stop start)
	
	let add_line ?(autocreate = false) ?(highlighted = false) ?(input = false) name line =
		let chan =
			try
				List.find begin fun ch -> ch.name = name end state.channels
			with Not_found ->
				if autocreate then begin
					add_channel name;
					ExtList.List.last state.channels
				end else raise Not_found
		in
		(* scroll a line first *)
		let len = Array.length chan.lines in
		Array.blit chan.lines 1 chan.lines 0 (len - 1);
		if highlighted then begin
			chan.lines.(len - 1) <- Printf.sprintf "\027[33;1m%s\027[m" line;
			chan.highlighted <- true && chan <> state.active;
		end else
			chan.lines.(len - 1) <- line;
		(* we have activity on this channel *)
		if chan <> state.active && chan.highlighted = false then
			chan.activity <- true;
		let start = Asm.rdtsc () in
		if input then
			redraw ()
		else begin
			status ();
			active_channel ();
			printf "\027[37;44;1m";
		end;
		let stop = Asm.rdtsc () in
		Printf.eprintf "add_line: %Ld (redraw = %B)\n" (Int64.sub stop start) input
	
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
	
	let init () =
		let status = mk_status_channel () in
		state.channels <- [status];
		state.active <- status
end

(* freenode: 216.165.191.52, or a list of many, many others... :P *)
let freenode = NetworkProtocolStack.IPv4.Addr (216, 155, 130, 130)

let clean_nick user =
	try
		String.sub user 0 (String.index user '!')
	with Not_found -> user

let status = "[status]"

let run server port nick pass channel' =
	UI.init ();
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
				let parts = String.nsplit line " " in
				if parts <> [] then begin
				let cmd, rest =
					try String.split line " " with _ -> line, ""
				in match String.nsplit line " " with
				| "/me" :: _ ->	(* CTCP action *)
					writef "PRIVMSG %s :\001ACTION %s \001" (channel ()) rest;
					UI.add_line ~input:true (channel ()) (Printf.sprintf " * %s %s" nick rest)
				| "/nick" :: new_nick :: _ ->	(* Change nickname *)
					writef "NICK %s" new_nick;
					(* need to propagate the change... *)
				| "/join" :: channels -> 	(* Join channel(s) *)
					List.iter (fun channel ->
						UI.add_channel channel;
						writef "JOIN %s" channel) channels;
					UI.redraw ()
				| "/part" :: channels ->	(* Part channel(s) *)
					List.iter (fun channel ->
						writef "PART %s" channel) channels;
				| "/mode" :: _ ->	(* Channel mode *)
					writef "MODE %s" rest;
				(*| "/um" ->	(* User mode *)
					(* need the current nickname which might have changed
						writef ":MODE %s %s" nick (String.slice ~first:4 line)
					*)
					Printf.printf "Not implemented"*)
				| "/quit" :: _ ->	(* Quit IRC *)
					writef "QUIT :%s" rest;
					TCP.close socket;
					issued_quit := true;
					printf "\027[0m\027[J";
					Thread.yield ();
				| "/cycle" :: _ ->
					UI.cycle_channels ()
				| "/query" :: user :: [] ->
					UI.add_channel user;
					UI.redraw ();
				| "/?" :: [] ->	(* Help! *)
					UI.add_line ~input:true status "Help!" (*"/me for CTCP action, /n for nickname, /j for join, /p for part (type /p #chan :REASON if you want a reason), /m for channel modes, /um for user modes, /q for quit, /? for this help"*)
				| _ ->	(* Anything else is a message. *)
					writef "PRIVMSG %s :%s" (channel ()) line;
					UI.add_line ~input:true (channel ()) (Printf.sprintf " <%s> %s" nick line)
			end else begin
				writef "PRIVMSG %s :%s" (channel ()) line;
				UI.add_line ~input:true (channel ()) (Printf.sprintf " <%s> %s" nick line)
			end
			with Not_found -> UI.redraw ()
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
						if (clean_nick target) = nick then
							UI.add_line ~autocreate:true ~highlighted:true (sender s) (Printf.sprintf " <%s> %s" (sender s) msg)
						else
							UI.add_line ~highlighted (sender target) (Printf.sprintf " <%s> %s" (sender s) msg);
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
				printf "Error: %s (%s)\n" (Printexc.to_string ex) line
	done

let nick = ref "snowflake-os"
let pass = ref ""

let test () =
	if !nick = "snowflake-os" && !pass = "" then
		Printf.printf "irc: specify password for snowflake-os or use a different nick\n"
	else run freenode 8000 !nick !pass "#snowflake"

open Shell
open Arg

let init () =
	add_command "irc" test [
		"-nick", Set_string nick, " <nickname>";
		"-pass", Set_string pass, " <password";
	]
