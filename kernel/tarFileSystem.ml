
(* Tar File System -- kind of similar to Tar File, but without the entire file in memory *)

type record = { ofs : int; size : int }

module Order = struct
	type t = string list
	type key = record option
	type index = string
	let to_index = (fun x -> x)
	let from_index = (fun x -> x)
end

module TarFile = Trie.Make(Order)

type entry = File | Directory | Link | Unknown

let octal s = Scanf.sscanf s "%o" (fun x -> x)
let round n = if n mod 512 = 0 then n else n + 512 - n mod 512

let split_on_slash path =
	let len = String.length path in
	let rec split ix =
		try
			let slash = String.index_from path ix '/' in
			if slash = 0 then split (slash + 1)
			else String.sub path ix (slash - ix) :: split (slash + 1)
		with Not_found -> String.sub path ix (len - ix) :: []
	in
	if len = 0 then []
	else if len = 1 && path.[0] = '/' then []
	else split 0

module IDE_stuff = struct
	let read offset length =
		let l = (length + 512) / 512 in
		let s = offset / 512 in
		(*Vt100.printf "reading data: %d (%d), %d (%d)\n" s offset l length;*)
		let data = IDE.read_disk 0x00 s l in
		(*for i = 0 to 511 do
			if i mod 16 = 0 then Vt100.printf "\n%03x: " (i/16);
			Vt100.printf "%c" data.[i];
		done;*)
		String.sub data (offset mod 512) length
end

let trie () =
	let trie = TarFile.empty () in
	let size = 861818880 in
	let read ofs len =
		let len =
			if ofs >= size then 0
			else min (size - ofs) len in
		let buf = IDE_stuff.read ofs len in
		try
			String.sub buf 0 (String.index buf '\000')
		with Not_found -> buf
	in
	let name ofs = "/" ^ read ofs 100 in
	let size ofs = octal (read (ofs + 124) 12) in
	let magic ofs = read (ofs + 257) 8 in
	let kind ofs = 
		let s = read (ofs + 256) 1 in
		if String.length s = 0 then File
		else match s.[0] with
			| '\000' | '0' | '7' -> File
			| '5' -> Directory
			| '1' | '2' -> Link
			| _ -> Unknown
	in
	
	let rec make ofs =
		let real_name = name ofs in
		let name = split_on_slash real_name in
		if name = [] then TarFile.insert [] None trie
		else begin
			let magic = magic ofs in
			if magic <> "ustar  " && magic <> "       " then
				raise (Sys_error ("Invalid TAR file: " ^ magic ^ "."));
			match kind ofs with
			| File ->
				let r = { ofs = ofs + 512; size = size ofs } in
				(* does multiple insertions =( ah well *)
				ignore (List.fold_left (fun path n ->
					let path = path @ [n] in
					if path = name then TarFile.insert path (Some r) trie
					else TarFile.insert path None trie; path) [] name);
				make (round (r.ofs + r.size))
			| Directory ->
				TarFile.insert name None trie;
				make (ofs + 512)
			| _ -> raise (Sys_error "Unknown type of TAR entry")
		end
	in make 0;
	trie

let rec nub = function
| x :: y :: rest when x = y -> nub (y::rest)
| x :: rest -> x :: nub rest
| [] -> []

type t = {
	trie : TarFile.t;
}

let open_tar_file () =
	{
		trie = trie ();
	}

open Bigarray

let open_a_file t path =
	let path = split_on_slash path in
	match TarFile.find_empty (TarFile.restrict path t.trie) with
		| None -> failwith "File not found"
		| Some r ->
			(* easier just to have a massive bigarray and fill it all up for now *)
			let ba = Array1.create int8_unsigned c_layout r.size in
			let n = r.ofs / 512 in
			for i = 0 to r.size / 512 - 1 do
				(* be inefficient, read one sector at a time *)
				let buf = IDE.read_disk 0x00 (i+n) 1 in
				Array1.blit_from_string buf
					(Array1.sub ba (i*512) 512);
			done;
			(* not sure if this works *)
			if r.size mod 512 <> 0 then begin
				let rem = r.size mod 512 in
				let buf = IDE.read_disk 0x00 ((r.size / 512)+n) 1 in
				Array1.blit_from_string
					(String.sub buf 0 rem)
					(Array1.sub ba (r.size - rem) rem);
			end;
			ba
			(*IO.from_in_channel(object
				method input buf ofs len =
					if !pos = r.size then 0 (* no more data *)
					else begin
						let l = min len (r.size - !pos) in
						let data = IDE_stuff.read (!pos+r.ofs) l in
						pos := !pos + l;
						String.blit data 0 buf ofs l;
						l
					end
				method close_in () = ()
			end)*)

let dir_list t path =
	let path = split_on_slash path in
	let trie = TarFile.restrict path t.trie in
	match TarFile.find_empty trie with
		| Some _ -> failwith "Not a directory"
		| None ->
			let entries = TarFile.fold (fun elt -> List.hd elt) trie in
			nub (List.sort compare entries)

open Arg
open Shell

let path = ref ""

let open_file = ref (fun _ -> failwith "no file system present")

let init () =
	let tarfs = open_tar_file () in
	open_file := open_a_file tarfs;
	let print_list () =
		Vt100.printf "Listing for: %s\n" ("/" ^ !path);
		List.iter begin fun n -> Vt100.printf "  %s\n" n end (dir_list tarfs !path);
		path := "";
	in
	add_command "ls" print_list [
		"-path", Set_string path, " Directory to list"
	]

let open_file path = !open_file path
