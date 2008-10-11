
(* ELF -- The Executable and Linking Format *)

(*
	Linking View:
		ELF Header
		Program Header Table (optional)
		Section 1
		...
		Section n
		...
		Section Header Table
	
	Execution View:
		ELF Header
		Program Header Table
		Segment 1
		Segment 2
		....
		Section Header Table (optional)
*)

type header = {
	version : int;
	file_type : file_type;
	entry : int32;
	phoff : int32;
	shoff : int32;
	header_flags : int32; (* 16 *)
	ehsize : int; (* 16 *)
	phentsize : int; (* 16 *)
	phnum : int; (* 16 *)
	shentsize : int;
	shnum : int;
	section_headers : section_header array;
	shstrndx : int; (* 16 *)
	string_table : string;
}
and file_type
	= Relative (* 1 *)
	| Executable (* 2 *)
	| Shared (* 3 *)
and section_header = {
	section_name : int (* offset into string table *);
	section_type : section_type;
	section_flags : int32; (* 16 *)
	section_addr : int32;
	section_offset : int;
	section_size: int; (* 16 *)
	section_link: int; (* 16 *)
	section_info: int32; (* 16 *)
	section_addralign: int; (* 16 *)
	section_entsize: int32; (* 16 *)
}
and section_type
	= Null (* 0 *)
	| ProgBits (* 1 *)
	| SymTab (* 2 *)
	| StrTab (* 3 *)
	| RelA (* 4 *)
	| Hash (* 5 *)
	| Dynamic (* 6 *)
	| Note (* 7 *)
	| NoBits (* 8 *)
	| Rel (* 9 *)
	| ShLib (* 10 *)
	| Dynsym (* 11 *)
	| Other of int

type elf = { header : header; data : Bitstring.t }

type t
	= Object of elf
	| Archive of (string * elf) list

(*
	section header table entry 0:
	name = 0, type = null, flags = 0, addr = 0, offset = 0,
	size = 0, link = SHN_UNDEF, info = 0, addralign = 0,
	entsize = 0
*)

(*
	section_flags:
	write = 0x1, alloc = 0x2, execinstr = 0x4, maskproc = 0xF0000000
*)

type symbol_table_entry = {
	st_name : int;
	st_value : int32;
	st_size: int;
	st_info: int;
	st_other: int;
	st_shndx: int;
}

type rel = {
	r_offset: int32;
	r_info: int32;
}
type rela = {
	ra_offset: int32;
	ra_info: int32;
	ra_addend: int32; (* signed *)
}

let rec parse_symbol_table acc bits =
	if Bitstring.bitstring_length bits = 0 then
		Array.of_list (List.rev acc)
	else begin
		bitmatch bits with
		| { name : 32 : littleendian;
			value : 32 : littleendian;
			size : 32 : littleendian;
			info : 8 : littleendian;
			other : 8 : littleendian;
			shndx : 16 : littleendian;
			rest : -1 : bitstring } ->
				parse_symbol_table ({
					st_name = Int32.to_int name;
					st_value = value;
					st_size = Int32.to_int size;
					st_info = info;
					st_other = other;
					st_shndx = shndx } :: acc) rest

let copy i = Int32.add Int32.zero i

exception Not_elf_file
exception Not_archive

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
		section_name = Int32.to_int s_name;
		section_type = begin match Int32.to_int (copy s_type) with
			| 0 -> Null
			| 1 -> ProgBits
			| 2 -> SymTab
			| 3 -> StrTab
			| 4 -> RelA
			| 5 -> Hash
			| 6 -> Dynamic
			| 7 -> Note
			| 8 -> NoBits
			| 9 -> Rel
			| 10 -> ShLib
			| 11 -> Dynsym
			| n -> Other n
			end;
		section_flags = copy s_flags;
		section_addr = copy s_addr;
		section_offset = Int32.to_int s_offset;
		section_size = Int32.to_int s_size;
		section_link = Int32.to_int s_link;
		section_info = copy s_info;
		section_addralign = Int32.to_int s_addralign;
		section_entsize = copy s_entsize;
	}

let rec parse_section_headers acc num size bits =
	if num = 0 then Array.of_list (List.rev acc)
	else
	bitmatch bits with
	| { data : size*8 : bitstring; rest : -1 : bitstring }
		-> parse_section_headers (parse_section_header data :: acc) (num-1) size rest
	| { _ } -> Array.of_list (List.rev acc)
	
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
			(Bitstring.subbitstring bits (Int32.to_int shoff * 8) (shentsize * shnum * 8)) in
		let string_table = Bitstring.string_of_bitstring
			(Bitstring.subbitstring bits
				(section_headers.(shstrndx).section_offset * 8)
				(section_headers.(shstrndx).section_size * 8))
		in
		{
			data = bits;
				header =  {
				version = Int32.to_int version;
				file_type = begin match elf_type with
					| 1 -> Relative
					| 2 -> Executable
					| 3 -> Shared
					| _ -> failwith "Unsupported ELF file type"
					end;
				entry = entry;
				phoff = phoff;
				shoff = shoff;
				header_flags = flags;
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

let build_lookup_table ht s off =
	build_lookup_table ht s off (String.length s)

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
			build_lookup_table ht (Bitstring.string_of_bitstring filedata) 0;
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
		Archive (parse_archive_members [] (Hashtbl.create 7) rest)
	| { _ } -> raise Not_archive

let parse filename bits =
	begin
		try
			Object (parse_elf_header bits)
		with Not_elf_file -> begin
		try
			parse_archive bits
		with Not_archive ->
			failwith "Not an ELF or archive file"
		end
	end

let print_section_type () = function
	| Null -> Printf.sprintf "%-12s" "NULL"
	| ProgBits -> Printf.sprintf "%-12s" "PROGBITS"
	| SymTab -> Printf.sprintf "%-12s" "SYMTAB"
	| StrTab -> Printf.sprintf "%-12s" "STRTAB"
	| RelA -> Printf.sprintf "%-12s" "RELA"
	| Hash -> Printf.sprintf "%-12s" "HASH"
	| Dynamic -> Printf.sprintf "%-12s" "DYNAMIC"
	| Note -> Printf.sprintf "%-12s" "NOTE"
	| NoBits -> Printf.sprintf "%-12s" "NOBITS"
	| Rel -> Printf.sprintf "%-12s" "REL"
	| ShLib -> Printf.sprintf "%-12s" "SHLIB"
	| Dynsym -> Printf.sprintf "%-12s" "DYNSYM"
	| Other x -> Printf.sprintf "unknown <%d>" x

let get_string strtab off =
	String.sub strtab off (String.index_from strtab off '\000' - off)

let truncate len s =
	if String.length s <= len then s
	else String.sub s 0 len

let print_section_header strtab i h =
	Vt100.printf " [%2d] %-16s  %a   %08lx %06x %06x                 \n"
		i (truncate 16 (get_string strtab h.section_name))
		print_section_type h.section_type h.section_addr
		h.section_offset h.section_size

let print_type () = function
	| Relative -> "relocatable"
	| Executable -> "executable"
	| Shared -> "shared"

let print_header h =
	let h = h.header in
	Vt100.printf "ELF Header:\n";
	Vt100.printf " Type: %a\n Entry point address: 0x%lx\n Start of program headers: %ld (bytes into file)\n Start of section headers: %ld (bytes into file)\n"
		print_type h.file_type h.entry h.phoff h.shoff;
	Vt100.printf " Flags: 0x%lx\n Size of this header: %d (bytes)\n Size of program headers: %d (bytes)\n Number of program headers: %d\n Size of section headers: %d (bytes)\n Number of sections headers: %d\n Section header string table index: %d\n"
		h.header_flags h.ehsize h.phentsize h.phnum h.shentsize h.shnum h.shstrndx;
	Vt100.printf "Section Headers:\n";
	Vt100.printf " [Nr] Name              Type           Addr     Off    Size   ES Flg Lk Inf Al\n";
	Array.iteri (print_section_header h.string_table) h.section_headers


let print = function
	| Object elf -> print_header elf
	| Archive list ->
		Vt100.printf "Archive:\n";
		List.iter (fun (n,e) ->
			Vt100.printf "Member: %s\n" n;
			print_header e) list

module LinkKernel = struct

	let st_bind x = x asr 4
	let st_type x = x land 0xF
	let st_info b t = (b lsl 4) + (t land 0xF)

	let tar_file = lazy begin
			let data = Multiboot.open_module () in
			let str = String.create (Bigarray.Array1.dim data) in
			for i = 0 to String.length str - 1 do
				str.[i] <- data.{i}
			done;
			TarFile.open_tar_file str
		end

	let objs = lazy begin
			Array.map begin fun filename ->
				parse filename (Bitstring.bitstring_of_string
					(TarFile.read_file (Lazy.force tar_file) filename))
			end LinkerTest.input_files
		end
	
	let defined_symbols = Hashtbl.create 10240
	let undefined_symbols = Hashtbl.create 1024
	let collected_objects = ref []
	let available_libs = ref []
	let found_symbols = ref 0
	let already_undefined_symbols = ref 0
	let collided_symbols = ref 0
	
	let find_symbols strtab symtab =
		let u = Hashtbl.create 7 in
		let d = Hashtbl.create 7 in
		Array.iter begin fun entry ->
			let name = get_string strtab entry.st_name in
			if entry.st_shndx = 0 then
				Hashtbl.add u name entry
			else
				Hashtbl.add d name entry
		end symtab;
		(u, d)
	
	let add_symbols u d =
		Hashtbl.iter begin fun n e ->
			if Hashtbl.mem defined_symbols n = false then begin
				Hashtbl.add defined_symbols n e;
				Hashtbl.remove undefined_symbols n
			end
		end d;
		Hashtbl.iter begin fun n e ->
			if Hashtbl.mem defined_symbols n = false
				&& Hashtbl.mem undefined_symbols n = false
			then Hashtbl.add undefined_symbols n e
		end u
	
	let collect_object elf =
		let symtbl_h = List.find begin fun h ->
			String.compare (get_string elf.header.string_table h.section_name) ".symtab" = 0
		end (Array.to_list elf.header.section_headers) in
		let symtbl = parse_symbol_table [] begin
			Bitstring.subbitstring elf.data (symtbl_h.section_offset * 8) (symtbl_h.section_size * 8)
		end in
		(* got the symbol table *)
		let string_table = Bitstring.string_of_bitstring
			(Bitstring.subbitstring elf.data
				(elf.header.section_headers.(symtbl_h.section_link).section_offset * 8)
				(elf.header.section_headers.(symtbl_h.section_link).section_size * 8))
		in
		find_symbols string_table symtbl
	
	let collect () =
		let objs = Lazy.force objs in
		Array.iter begin function
			| Object elf ->
				collected_objects := elf :: !collected_objects;
				let u, d = collect_object elf in
				add_symbols u d
			| Archive list ->
				available_libs := list :: !available_libs
		end objs
	
	let collect_lib list =
		List.iter begin fun (name,elf) ->
			try
				let u, d = collect_object elf in
				if Hashtbl.fold begin fun n _ b ->
					if (b = true) || (Hashtbl.mem undefined_symbols n) = true then
						true
					else b
				end d false = true then begin
					add_symbols u d;
					collected_objects := elf :: !collected_objects
				end
			with Not_found -> ()
		end list
	
	let rec collect_from_libs () =
		let count = Hashtbl.length undefined_symbols in
		List.iter collect_lib !available_libs;
		if Hashtbl.length undefined_symbols <> count then
			collect_from_libs ()
		else () (* fix point reached *)
	
	let combine_sections tbl header =
		Array.iter begin fun s_header ->
			let name = get_string header.string_table s_header.section_name in
			try
				let entries = Hashtbl.find tbl name in
				Hashtbl.replace tbl name (s_header :: entries)
			with Not_found ->
				Hashtbl.add tbl name [s_header]
		end header.section_headers
	
	(*
		Sections to be in the output file:
		.mb_header
		.hash
		.dynsym
		.dynstr
		.dynamic
		.gnu.version
		.gnu.version_d
		.gnu.version_r
		.got.plt
		.eh_frame
		.text ( .text .text.* .gnu.linkonce.t.* )
		PROVIDE etext
		.rodata ( .rodata .rodata.* .gnu.linkonce.d.* )
		PROVIDE edata
		ALIGN(0x1000)
		.bss ( .bss.pagealigned, .bss .bss.* .gnu.linkonce.b.*, COMMON )
		PROVIDE end
		.stab
		.stabstr
		.stab.excl
		.stab.exclstr
		.stab.index
		.stab.indexstr
		.comment
		.debug
		.line
		.debug_srcinfo
		.debug_sfnames
		.debug_aranges
		.debug_pubnames
		.debug_info ( .debug_info .gnu.linkonce.wi.* )
		.debug_abbrev
		.debug_line
		.debug_frame
		.debug_str
		.debug_loc
		.debug_macinfo
		.debug_weaknames
		.debug_funcnames
		.debug_typenames
		.debug_varnames
		DISCARD .note.GNU-stack
	*)
	
	(*
		Sections that are in snowflake.native: (when stripped)
		.mb_header, LONG(0)
		.eh_frame
		.text
		PROVIDE (_etext)
		.rodata
		.data
		_edata, PROVIDE(edata)
		ALIGN(0x1000)
		__bss_start
		.bss
		_end, PROVIDE(end)
		.comment
		.shstrtab
	*)
	
	let get_size list =
		List.fold_right begin fun entry size ->
			let newsize = entry.section_size in
			if size = 0 then
				newsize
			else if entry.section_addralign = 0 then
				size + newsize
			else begin
				if size mod entry.section_addralign = 0 then
					size + newsize
				else
					((size / entry.section_addralign + 1) * entry.section_addralign) + newsize
			end
		end list 0
		
	let link () =
		let objs = Lazy.force objs in
		Vt100.printf "Collecting objects...\n";
		collect ();
		collect_from_libs ();
		Vt100.printf "Undefined symbols:\n";
		Hashtbl.iter begin fun n _ ->
			Vt100.printf "  %s\n" n
		end undefined_symbols;
		Vt100.printf "Calculating section sizes...\n";
		let sections = Hashtbl.create 10 in
		List.iter begin fun elf ->
			combine_sections sections elf.header
		end !collected_objects;
		Hashtbl.iter begin fun name list ->
			Vt100.printf "Section %-32s: %06x bytes\n" name (get_size list)
		end sections;
		Vt100.printf "This is where we'd start doing linking...\n"

end
