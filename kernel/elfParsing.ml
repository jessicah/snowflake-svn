
open ElfTypes

let copy i = Int32.add Int32.zero i
let to_int = Int32.to_int

(* Symbol Tables *)

let rec parse_symbol_table acc bits =
	if Bitstring.bitstring_length bits = 0 then
		Array.of_list (List.rev acc)
	else
		bitmatch bits with
		| { name : 32 : littleendian;
			value : 32 : littleendian;
			size : 32 : littleendian;
			info : 8 : littleendian;
			other : 8 : littleendian;
			shndx : 16 : littleendian;
			rest : -1 : bitstring } ->
				parse_symbol_table ({
					sm_name = to_int name;
					sm_value = to_int value;
					sm_size = to_int size;
					sm_info = info;
					sm_other = other;
					sm_shndx = shndx } :: acc) rest

let parse_symbol_table bits = parse_symbol_table [] bits

(* Section Headers *)

let parse_section_header bits =
	bitmatch bits with
	| { s_name : 32 : littleendian;
		s_type : 32 : littleendian;
		s_flags : 32 : littleendian;
		s_addr : 32 : littleendian;
		s_offset : 32 : littleendian;
		s_size : 32 : littleendian;
		s_link : 32 : littleendian;
		s_info : 32 : littleendian;
		s_addralign : 32 : littleendian;
		s_entsize : 32 : littleendian
	} -> {
		st_name = to_int s_name;
		st_type = begin let n = to_int s_type in
				if n < 12 then Obj.magic n else Other n
			end;
		st_flags = to_int s_flags;
		st_addr = to_int s_addr;
		st_offset = to_int s_offset;
		st_size = to_int s_size;
		st_link = to_int s_link;
		st_info = to_int s_info;
		st_addralign = to_int s_addralign;
		st_entsize = to_int s_entsize;
	}

let rec parse_section_headers acc num size bits =
	if num = 0 then Array.of_list (List.rev acc)
	else
	bitmatch bits with
	| { data : size*8 : bitstring; rest : -1 : bitstring }
		-> parse_section_headers (parse_section_header data :: acc) (num-1) size rest
	| { _ } -> Array.of_list (List.rev acc)

(* ELF Object Files *)

let parse_elf_header bits =
	bitmatch bits with
	| { "\x7FELF\x01\x01" : 6*8 : string;
		_ : 10*8 : bitstring;
			(* 0x7F 'E' 'L' 'F' *)
			(* 1 = ELFCLASS32 *)
			(* 1 = ELFDATA2LSB *)
			(* == version *)
			(* beginning of unused bytes in ident that should be ignored *)
		elf_type : 16 : littleendian; (* REL = 1, EXEC = 2, DYN = 3 *)
		machine : 16 : littleendian; (* EM_386 = 3 *)
		version : 32 : littleendian;
		entry : 32 : littleendian;
		phoff: 32 : littleendian;
		shoff : 32 : littleendian;
		flags : 32 : littleendian;
		ehsize : 16 : littleendian;
		phentsize : 16 : littleendian;
		phnum : 16 : littleendian;
		shentsize : 16 : littleendian;
		shnum : 16 : littleendian;
		shstrndx : 16 : littleendian
	} ->
		let section_headers = parse_section_headers [] shnum shentsize
			(Bitstring.subbitstring bits (to_int shoff * 8) (shentsize * shnum * 8)) in
		let string_table = Bitstring.string_of_bitstring
			(Bitstring.subbitstring bits
				(section_headers.(shstrndx).st_offset * 8)
				(section_headers.(shstrndx).st_size * 8))
		in
		{
			data = bits;
			header = {
				version = to_int version;
				file_type = begin match elf_type with
					| 1 -> Relative
					| 2 -> Executable
					| 3 -> Shared
					| _ -> failwith "Unsupported ELF file type"
					end;
				entry = to_int entry;
				phoff = to_int phoff;
				shoff = to_int shoff;
				header_flags = to_int flags;
				ehsize = ehsize;
				phentsize = phentsize;
				phnum = phnum;
				shentsize = shentsize;
				shnum = shnum;
				section_headers = section_headers;
				shstrndx = shstrndx;
				string_table = string_table;
			}
		}
	| { _ } -> raise Not_elf_file

(* Archive Files (gnu ar) *)

let index_from s o c =
	try Some (String.index_from s o c)
	with Not_found -> None

let rec build_lookup_table ht s off len =
	if off >= len then ()
	else match index_from s off '\n' with
		| Some i ->
			if i = off then ()
			else begin
				let sub = String.sub s off (i-1-off) in
				Hashtbl.add ht off (sub ^ "/");
				build_lookup_table ht s (i + 1) len
			end
		| None -> ()

let build_lookup_table ht s =
	build_lookup_table ht s 0 (String.length s)

let fix_name name =
	String.sub name 0 (String.index name '/')

let rec parse_archive_members acc ht bits =
	if Bitstring.bitstring_length bits = 0 then acc
	else bitmatch bits with
	| { name : 16 * 8 : string;
		_ : 12 * 8 : bitstring; (* date *)
		_ : 6 * 8 : bitstring; (* uid *)
		_ : 6 * 8 : bitstring; (* gid *)
		_ : 8 * 8 : bitstring; (* mode *)
		size : 10 * 8 : string;
		0x600A : 2 * 8;
		filedata : (Scanf.sscanf size "%d" (fun x -> x)) * 8 : bitstring;
		rest : -1 : bitstring } ->
		begin match name with
		| "/               " ->
			parse_archive_members acc ht rest
		| "//              " ->
			(* build the lookup table *)
			build_lookup_table ht (Bitstring.string_of_bitstring filedata);
			parse_archive_members acc ht rest
		| name when name.[0] = '/' ->
			(* need to do a lookup *)
			let name = fix_name (Hashtbl.find ht (Scanf.sscanf name "/%d" (fun x -> x))) in
			begin try
				let member = (name, parse_elf_header filedata) in
				parse_archive_members (member :: acc) ht rest
			with Not_elf_file ->
				parse_archive_members acc ht rest
			end
		| name ->
			begin try
				let member = (fix_name name, parse_elf_header filedata) in
				parse_archive_members (member :: acc) ht rest
			with Not_elf_file ->
				parse_archive_members acc ht rest
			end
		end
	| { 0x0a : 8;
		rest : -1 : bitstring } -> parse_archive_members acc ht rest

let parse_archive bits =
	bitmatch bits with
	| { "!<arch>\n" : 8*8 : string;
		rest : -1 : bitstring } ->
		(parse_archive_members [] (Hashtbl.create 7) rest)
	| { _ } -> raise Not_archive

(* Parse an object file (foo.o or foo.a) *)

let parse filename bits =
	begin
		try
			Object (parse_elf_header bits)
		with Not_elf_file -> begin
		try
			Archive (parse_archive bits)
		with Not_archive ->
			raise Not_elf_or_archive_file
		end
	end
