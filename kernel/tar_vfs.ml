
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
		let disk = IDE.get IDE.Primary IDE.Master in
		let data = IDE.read_disk disk s l in
		(*for i = 0 to 511 do
			if i mod 16 = 0 then Vt100.printf "\n%03x: " (i/16);
			Vt100.printf "%c" data.[i];
		done;*)
		String.sub data (offset mod 512) length
	
	let read_sector n =
		let disk = IDE.get IDE.Primary IDE.Master in
		IDE.read_disk disk n 1
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

(* The VFS Implementation for tar files...???? *)

module FileSystem (*: Vfs.FileSystem*) = struct
	type inode = {
			trie : TarFile.t;
			mutable offset : int;
			mutable position : int;
			mutable length : int;
		}
	
	module Ops (*: Vfs.Inode*) = struct
		type t = inode
		
		let open_in inode =
			match TarFile.find_empty inode.trie with
				| None -> failwith "not a file"
				| Some r ->
					inode.offset <- r.ofs;
					inode.position <- 0;
					inode.length <- r.size
		
		let close_in _ = ()
		let flush_in _ = ()
		let input_byte inode =
			if inode.position >= inode.length then raise End_of_file;
			(* find disk sector offset+position in *)
			let sector = (inode.offset + inode.position) / 512 in
			let offset = inode.position mod 512 in
			(*let buffer = IDE.read_disk 0x00 sector 1 in*)
			let buffer = IDE_stuff.read_sector sector in
			Char.code buffer.[offset]
		
		let input_bytes inode obuf ofs len =
			if inode.position >= inode.length then 0
			else begin
				let n = (inode.offset + inode.position) / 512 in
				let s = min len (inode.length - inode.position - len) in
				for i = 0 to s / 512 - 1 do
					(*let buf = IDE.read_disk 0x00 (i+n) 1 in*)
					let buf = IDE_stuff.read_sector (i+n) in
					String.blit buf 0 obuf (i * 512 + ofs) 512;
				done;
				if s mod 512 <> 0 then begin
					let rem = s mod 512 in
					(*let buf = IDE.read_disk 0x00 ((s / 512) + n) 1 in*)
					let buf = IDE_stuff.read_sector ((s / 512) + n) in
					String.blit buf 0 obuf (String.length obuf - rem) rem;
				end;
				(* return *) s
			end
		
		let seek_in inode npos =
			if npos > inode.length || npos < 0 then raise (Invalid_argument "npos");
			inode.position <- npos
		
		let pos_in inode = inode.position
		
		let length_in inode = inode.length
		
		open Vfs
		
		let open_out _ = raise Not_supported
		let close_out _ = raise Not_supported
		let flush_out _ = raise Not_supported
		let output_byte _ = raise Not_supported
		let output_bytes _ _ _ _ = raise Not_supported
		let seek_out _ _ = raise Not_supported
		let pos_out _ = raise Not_supported
		let length_out _ = raise Not_supported
		
		let of_abstract_inode : abstract_inode -> t = Obj.magic
		let to_abstract_inode : t -> abstract_inode = Obj.magic
	end
	
	let trie = lazy (trie ())
	
	let walk path = try
			Some (Ops.to_abstract_inode {
				trie = TarFile.restrict_direct path (Lazy.force trie);
				offset = 0; position = 0; length = 0;
			})
		with Not_found -> None
	
	let is_directory inode =
		match TarFile.find_empty (Ops.of_abstract_inode inode).trie with
			None -> true | _ -> false		
	
	let rec nub = function
		| x :: y :: rest when x = y -> nub (y::rest)
		| x :: rest -> x :: nub rest
		| [] -> []
	
	let read_dir inode =
		match TarFile.find_empty (Ops.of_abstract_inode inode).trie with
		| Some _ -> failwith "Not a directory"
		| None ->
			let entries = TarFile.fold (fun elt -> List.hd elt) (Ops.of_abstract_inode inode).trie in
			nub (List.sort compare entries)
end

let init () =
	Vfs.mount (module FileSystem : Vfs.FileSystem) "tarfs"
