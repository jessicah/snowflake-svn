
(* streaming audio/daap client *)

module IPv4 = NetworkProtocolStack.IPv4

let kprintf = Printf.kprintf

type config = {
	mutable host : IPv4.addr;
	mutable port : int;
	mutable session_id : int32;
	mutable revision_id : int32;
}

let config = {
		host = IPv4.invalid; port = 3689;
		session_id = -1l; revision_id = -1l;
	}

let req url =
	HTTP.request url [] config.host config.port

let req_stream url =
	HTTP.open_stream url config.host config.port

let login () =
	let _ = req "/server-info" in
	let session_id = DAAP.parse_login (req "/login") in
	let revision_id = DAAP.parse_update (kprintf req "/update?session-id=%ld" session_id) in
	config.session_id <- session_id;
	config.revision_id <- revision_id

let databases () =
	DAAP.output_databases
		(kprintf req "/databases?session_id=%ld&revision_id=%ld"
			config.session_id config.revision_id)

let songlist database =
	DAAP.output_songs
		(kprintf req "/databases/%d/items?music&session-id=%ld&revision-id=%ld"
			database config.session_id config.revision_id)

open Bigarray

let playsong database filename =
	try
	let filename = ExtString.String.replace_chars begin function
		| ' ' -> "%20"
		| c -> String.make 1 c
	end filename in
	Vt100.printf "requesting: /databases/%d/items/%s?session-id=%ld\n"
		database filename config.session_id;
	let stream = kprintf req_stream "/databases/%d/items/%s?session-id=%ld"
		database filename config.session_id
	in
	
	let buf_size = 128 * 1024 in
	(* now need to feed the stream to the audio mixer *)
	let input_buffer = BlockIO.make
		(Array1.create int8_unsigned c_layout buf_size)
	in
	
	let string_buffer = String.make buf_size '\000' in
	
	(* process the first buffer, which _should_ include the wave header *)
	Vt100.printf "processing stream...\n";
	ignore (IO.really_input stream string_buffer 0 buf_size);
	Array1.blit_from_string string_buffer input_buffer.BlockIO.data;
	Vt100.printf "+";
	AudioMixer.play (AudioMixer.Wave.read input_buffer);
	
	(* now process all remaining buffers *)
	while true do
		Vt100.printf " ";
		ignore (IO.really_input stream string_buffer 0 buf_size);
		input_buffer.BlockIO.pos <- 0; (* reset buffer *)
		Array1.blit_from_string string_buffer input_buffer.BlockIO.data;
		Vt100.printf "-";
		AudioMixer.play_raw input_buffer;
	done
	with exn -> Vt100.printf "playsong: error: %s\n" (Printexc.to_string exn)

(* the shell interface *)

open Shell
open Arg

let set_host ipaddr =
	config.host <- NetworkStack.Shell.of_string ipaddr

let set_port port =
	config.port <- port

let db = ref 0
let filename = ref ""

let netplay () =
	playsong !db !filename
	
(* the entry point called by snowflake.ml to:
		1. add the commands to the shell
		2. cause this and other dependent code to get linked in *)
let init () =
	add_command "daap-config" login [
		"-server", String set_host,	" IP Address of DAAP server";
		"-port", Int set_port,			" Port of DAAP server (3689 by default)";
	];
	add_command "daap" ignore [
		"-databases", Unit databases, " List databases";
		"-songlist", Int songlist, " List songs in database";
	];
	add_command "netplay" netplay [
		"-database", Set_int db, " Database to use";
		"-filename", Set_string filename, " File to play";
	]
