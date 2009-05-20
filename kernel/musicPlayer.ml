
(* a lil music player -- eventually would have a file system for this to work with *)

open Arg
open Shell
open Bigarray

let bg = ref false
let name = ref ""
let files = ref []
let playing = ref false

let add_dir path =
	files := TarFileSystem.dir_list path
	
let play_file () =
	if !playing then
		failwith "already playing a file";
	(*if String.length !name = 0 then
		failwith "filename not specified";*)
	if !files = [] then
		failwith "no files to play";
	let spawn_thread = true in
	bg := false;
	ignore (Thread.create begin fun () ->
		playing := true;
		List.iter begin fun name ->
			let ba = try
					TarFileSystem.open_file name
				with Not_found -> failwith "file not found"
			in
			let blockIO = BlockIO.make ba in
			AudioMixer.play (AudioMixer.Wave.read blockIO);
		end !files;
		playing := false;
	end () "music player")

(* hack around linking *)

let init () =
	add_command "musicplayer" play_file [
		"-bg", Set bg, " Play in the background (ignored)";
		"-play", Set_string name, " File to play (ignored)";
		"-playdir", String add_dir, " Directory of files to play";
	]
