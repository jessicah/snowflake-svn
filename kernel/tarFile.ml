
(* Tar File *)

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

let trie string =
	let trie = TarFile.empty () in
	let size = String.length string in
	let read ofs len =
		let len =
			if ofs >= size then 0
			else min (size - ofs) len in
		let buf = String.make len '\000' in
		String.blit string ofs buf 0 len;
		try
			String.sub buf 0 (String.index buf '\000')
		with Not_found -> buf
	in
	let name ofs = "/" ^ read ofs 100 in
	let size ofs = octal (read (ofs + 124) 12) in
	let magic ofs = read (ofs + 257) 8 in
	let kind ofs = match string.[ofs+256] with
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
	data : string;
}

let open_tar_file data =
	{
		trie = trie data;
		data = data;
	}

let read_file t path = (* reads the entire file *)
	let path = split_on_slash path in
	match TarFile.find_empty (TarFile.restrict path t.trie) with
		| None -> failwith "File not found"
		| Some r -> String.sub t.data r.ofs r.size

let dir_list t path =
	let path = split_on_slash path in
	let trie = TarFile.restrict path t.trie in
	match TarFile.find_empty trie with
		| Some _ -> failwith "Not a directory"
		| None ->
			let entries = TarFile.fold (fun elt -> List.hd elt) trie in
			nub (List.sort compare entries)
