
(* a lil music player -- eventually would have a file system for this to work with *)

open Arg
open Shell
open Bigarray

let bg = ref false
let name = ref ""
let playing = ref false

let play_file () =
	if !playing then
		failwith "already playing a file";
	if String.length !name = 0 then
		failwith "filename not specified";
	let spawn_thread = true in
	bg := false;
	(* get the file, and build a block io thing to try play it *)
	(*let input = try
			TarFileSystem.open_file !name
		with Not_found -> failwith "file not found"
	in
	let size = 0x8_0000 in
	let blockIO = BlockIO.make
		(Array1.create int8_unsigned c_layout size)
	in
	let buffer = String.make size '\000' in
	let first_buffer = ref true in
	let is_done = ref false in
	ignore (Thread.create begin fun () ->
		playing := true;
		while not !is_done do
			let n = IO.really_input input buffer 0 size in
			Array1.blit_from_string buffer blockIO.BlockIO.data;
			blockIO.BlockIO.pos <- 0;
			if !first_buffer then begin
				AudioMixer.play (AudioMixer.Wave.read blockIO);
				first_buffer := false;
			end else begin
				AudioMixer.play_raw blockIO;
			end;
			if n < size then
				is_done := true;
		done;
		playing := false;
	end () "music player")*)
	let ba = try
			TarFileSystem.open_file !name
		with Not_found -> failwith "file not found"
	in
	let blockIO = BlockIO.make ba in
	ignore (Thread.create begin fun () ->
		playing := true;
		AudioMixer.play (AudioMixer.Wave.read blockIO);
		playing := false;
	end () "music player")

(* hack around linking *)

let init () =
	add_command "musicplayer" play_file [
		"-bg", Set bg, " Play in the background (ignored)";
		"-play", Set_string name, " File to play";
	]
