
open Arg
open Shell

open Ext2fs

let fs = ref None

let set_fs x = fs := Some x

let dirs = ref []

let init () =
	match !fs with
	| None -> ()
	| Some fs ->
		let did_it = ref false in
		let dirlist name =
			begin try
				let inode = fs.read_inode begin
					(List.find begin fun entry ->
							entry.name = name
						end !dirs).inode
					end in
				dirs := fs.read_dir inode;
				Vt100.printf "Directory Listing for %s:\n" name;
				List.iter begin fun entry ->
						Vt100.printf " %s\n" entry.name
					end !dirs
			with Not_found ->
				Vt100.printf "directory not found\n"
			end;
			did_it := true
		in
		let dirlist_def () =
			if !did_it = false then
				dirlist ".";
			did_it := false
			in
		add_command "ls" (*dirlist [
			"-name", Set_string name, " Directory to list";
		];*) dirlist_def [] ~anon:dirlist;
		(* init dirs *)
		dirs := fs.read_dir fs.metadata.root
