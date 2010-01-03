
open Bigarray
open BlockIO
open FileSystems
open Ext2fs

let init () =
	match !FileSystems.fs with
	| None -> ()
	| Some fs ->
		(* add a command to shell thing *)
		let did_it = ref false in
		let play name =
			begin try
				let inode = fs.read_inode begin
					(List.find begin fun entry ->
							entry.name = name && entry.file_type = 1
						end !FileSystems.dirs).inode
					end in
				(*let contents = fs.read_file inode in
				let ba = Array1.create int8_unsigned c_layout (String.length contents) in
				Array1.blit_from_string contents ba;*)
				(*let ba = fs.read_file_ba inode in
				Vt100.printf "play: trying to play %s...\n" name;
				AudioMixer.play begin
					AudioMixer.Wave.read begin
						BlockIO.make ba
					end
				end;*)
				Vt100.printf "play: trying to play %s....\n" name;
				(* use a buffer size of 65536 bytes *)
				let limit = inode.i_size in
				let rec loop pos =
					if pos = limit then () else begin
						let ba = fs.read_file_range_ba inode pos 262144 in
						if pos = 0 then begin
							AudioMixer.play begin
								AudioMixer.Wave.read begin
									BlockIO.make ba
								end
							end
						end else begin
							AudioMixer.play_raw begin
								BlockIO.make ba
							end
						end;
						loop (pos + Array1.dim ba)
					end
				in loop 0
			with Not_found ->
				Vt100.printf "play: file not found, or not a file\n"
			end;
			did_it := true
		in
		let play_def () =
			if !did_it = false then
				Vt100.printf "play: require a filename\n";
			did_it := true
		in
		Shell.add_command "play" play_def [] ~anon:play
