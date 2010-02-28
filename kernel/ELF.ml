
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
	st_name : int (* offset into string table *);
	st_type : section_type;
	st_flags : int; (* 16 *)
	st_addr : int32;
	st_offset : int;
	st_size: int; (* 16 *)
	st_link: int; (* 16 *)
	st_info: int32; (* 16 *)
	st_addralign: int; (* 16 *)
	st_entsize: int32; (* 16 *)
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
	sm_name : int;
	sm_value : int32;
	sm_size: int;
	sm_info: int;
	sm_other: int;
	sm_shndx: int;
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

let copy i = Int32.add Int32.zero i

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
					sm_name = Int32.to_int name;
					sm_value = copy value;
					sm_size = Int32.to_int size;
					sm_info = info;
					sm_other = other;
					sm_shndx = shndx } :: acc) rest

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
		st_name = Int32.to_int s_name;
		st_type = begin match Int32.to_int (copy s_type) with
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
		st_flags = Int32.to_int s_flags;
		st_addr = copy s_addr;
		st_offset = Int32.to_int s_offset;
		st_size = Int32.to_int s_size;
		st_link = Int32.to_int s_link;
		st_info = copy s_info;
		st_addralign = Int32.to_int s_addralign;
		st_entsize = copy s_entsize;
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
				(section_headers.(shstrndx).st_offset * 8)
				(section_headers.(shstrndx).st_size * 8))
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
		i (truncate 16 (get_string strtab h.st_name))
		print_section_type h.st_type h.st_addr
		h.st_offset h.st_size

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

	(*
		According to what I read by Ian Lance Taylor
		(http://www.airs.com/blog/archives/39), there are four steps
		that need to be done:
		
		1.	Read the input object files. Determine the length and type
			of the contents. Read the symbols.
		2.	Build a symbol table containing all the symbols, linking
			undefined symbols to their definitions.
		3.	Decide where all the contents should go in the output
			executable file, which means deciding where they should go
			in memory when the program runs.
		4.	Read the contents data and the relocations. Apply the
			relocations to the contents. Write the result to the
			output file.
	*)
	
(* Section is struct {
	string name;
	address vaddr;
	address size;
	uint align;
	
	blob data
} *)
type section_t = {
	section_name : string;
	mutable section_vaddr : int;
	mutable section_size : int;
	mutable section_align : int;
	section_data : Bitstring.t;
}

(* Symbol is struct {
	string name
	address value
	address size
	Section sect
	
	bool is_defined
	Symbol linked_symbol # if this symbol is undefined, this is the defined symbol it's linked to
} *)
type symbol_t = {
	symbol_name : string;
	symbol_value : int;
	symbol_size : int;
	symbol_sect : section_t option;
	(* if is_defined is same as if symbol_linked_to <> None *)
	symbol_linked_to : symbol_t option;
}

(* # sections in the final output file
sections = new Hashtable of Sections; *)
let the_sections = Hashtbl.create 7 (* key is string, value is section_t *)

(* foreach(section_in_input_files) {
	# Where sect is the current section
	
	# minimum section alignment
	if sections[sect.name].align < sect.align
		sections[sect.name].align = sect.align
	
	# Update the input section's address to the offset within the final object file
	# sect.base must be aligned to the section's minimum alignment (or better)
	# I've got no idea how non-zero address work out, so i'm ignoring them here
	section.vaddr = align(sections[section.name].size, sect.align)
	
	# Update the size of the output section
	sections[section.name].size = section.vaddr + section.size
} *)

let () = Hashtbl.iter begin fun name sect ->
		let the_sect = Hashtbl.find the_sections sect.section_name in
		(* fix alignment if needed *)
		if the_sect.section_align < sect.section_align then
			the_sect.section_align <- sect.section_align;
		(* what the heckle is 'section' *)
		()
	end the_sections
(*
# do "linking undefined symbols to their definitions." here

# Fix the address of every symbol
foreach(defined_symbol_in_input_files) {
	# sym is the symbol
	# sect is the symbol's section in the object file (same as sect above)
	
	sym.value += sect.vaddr + section.real_base_link_address
}

# do step 3 here, pretty much a case of just sorting sections by vaddr and ensuring no overlaps

# step 4!

foreach(section in sections) {
	section.blob = allocate(section.size)
	
	# Input sections that are the same as, or will be merged in to this section
	foreach(input_sections) {
		# input_section.vaddr is actually the offset within the final output section
		copy(input_section.data to (section.data + offset input_section.vaddr))
	}
}

# do relocations here

*)
	

	let st_bind x = x asr 4
	let st_type x = x land 0xF
	let st_info b t = (b lsl 4) + (t land 0xF)

	let tar_file = lazy begin
			let data = Multiboot.open_module () in
			let str = String.create (Bigarray.Array1.dim data) in
			for i = 0 to String.length str - 1 do
				str.[i] <- char_of_int data.{i}
			done;
			TarFile.open_tar_file str
		end

	let objs = lazy begin
			Array.map begin fun filename ->
				filename, parse filename (Bitstring.bitstring_of_string
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
	
	let print_symbol () sym =
		Printf.sprintf "value = %lx, size = %x, binding =  %x, type = %x, other = %x, section = %x"
			sym.sm_value sym.sm_size (st_bind sym.sm_info) (st_type sym.sm_info) sym.sm_other sym.sm_shndx
	
	let find_symbols strtab symtab =
		let u = Hashtbl.create 7 in
		let d = Hashtbl.create 7 in
		Array.iter begin fun entry ->
			let name = get_string strtab entry.sm_name in
			if entry.sm_shndx = 0 then
				Hashtbl.add u name entry
			else
				Hashtbl.add d name entry
		end symtab;
		(u, d)
	
	let add_symbols u d =
		Hashtbl.iter begin fun n e ->
			Vt100.printf "Symbol (d) %s: %a\n" n print_symbol e;
			if Hashtbl.mem defined_symbols n = false then begin
				Hashtbl.add defined_symbols n e;
				Hashtbl.remove undefined_symbols n
			end
		end d;
		Hashtbl.iter begin fun n e ->
			Vt100.printf "Symbol (u) %s: %a\n" n print_symbol e;
			if Hashtbl.mem defined_symbols n = false
				&& Hashtbl.mem undefined_symbols n = false
			then Hashtbl.add undefined_symbols n e
		end u
	
	let collect_object elf =
		let symtbl_h = List.find begin fun h ->
			String.compare (get_string elf.header.string_table h.st_name) ".symtab" = 0
		end (Array.to_list elf.header.section_headers) in
		let symtbl = parse_symbol_table [] begin
			Bitstring.subbitstring elf.data (symtbl_h.st_offset * 8) (symtbl_h.st_size * 8)
		end in
		(* got the symbol table *)
		let string_table = Bitstring.string_of_bitstring
			(Bitstring.subbitstring elf.data
				(elf.header.section_headers.(symtbl_h.st_link).st_offset * 8)
				(elf.header.section_headers.(symtbl_h.st_link).st_size * 8))
		in
		find_symbols string_table symtbl
	
	let collect () =
		let objs = Lazy.force objs in
		Array.iter begin fun (n,obj) -> match obj with
			| Object elf ->
				collected_objects := (n,elf) :: !collected_objects;
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
					collected_objects := (name,elf) :: !collected_objects
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
			let name = get_string header.string_table s_header.st_name in
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
		ENTRY __entrypoint
		SECTIONS
			0x00400000 + SIZEOF_HEADERS
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
			let newsize = entry.st_size in
			if size = 0 then
				newsize
			else if entry.st_addralign = 0 then
				size + newsize
			else begin
				if size mod entry.st_addralign = 0 then
					size + newsize
				else
					((size / entry.st_addralign + 1) * entry.st_addralign) + newsize
			end
		end list 0
	
	let section_header_table = "\000.mb_header\000.eh_frame\000.text\000.rodata\000.data\000.bss\000.comment\000.shstrtab\000"
	
	let section_headers = [|
		(* null *)
		{ st_name = 0; st_type = Null; st_flags = 0;
			st_addr = Int32.zero; st_offset = 0; st_size = 0;
			st_link = 0; st_info = Int32.zero;
			st_addralign = 0; st_entsize = Int32.zero };
		(* mb_header *)
		{ st_name = 1; st_type = ProgBits; st_flags = 0x2 (* A *);
			st_addr = Int32.zero; st_offset = 0; st_size = 0;
			st_link = 0; st_info = Int32.zero;
			st_addralign = 4; st_entsize = Int32.zero };
		(* eh_frame *)
		{ st_name = 12; st_type = ProgBits; st_flags = 0x2 (* A *);
			st_addr = Int32.zero; st_offset = 0; st_size = 0;
			st_link = 0; st_info = Int32.zero;
			st_addralign = 4; st_entsize = Int32.zero };
		(* text *)
		{ st_name = 22; st_type = ProgBits; st_flags = 0x6 (* AX *);
			st_addr = Int32.zero; st_offset = 0; st_size = 0;
			st_link = 0; st_info = Int32.zero;
			st_addralign = 16; st_entsize = Int32.zero };
		(* rodata *)
		{ st_name = 28; st_type = ProgBits; st_flags = 0x2 (* A *);
			st_addr = Int32.zero; st_offset = 0; st_size = 0;
			st_link = 0; st_info = Int32.zero;
			st_addralign = 8; st_entsize = Int32.zero };
		(* data *)
		{ st_name = 36; st_type = ProgBits; st_flags = 0x3 (* WA *);
			st_addr = Int32.zero; st_offset = 0; st_size = 0;
			st_link = 0; st_info = Int32.zero;
			st_addralign = 32; st_entsize = Int32.zero };
		(* bss *)
		{ st_name = 42; st_type = NoBits; st_flags = 0x3 (* WA *);
			st_addr = Int32.zero; st_offset = 0; st_size = 0;
			st_link = 0; st_info = Int32.zero;
			st_addralign = 32; st_entsize = Int32.zero };
		(* comment *)
		{ st_name = 47; st_type = ProgBits; st_flags = 0;
			st_addr = Int32.zero; st_offset = 0; st_size = 0;
			st_link = 0; st_info = Int32.zero;
			st_addralign = 1; st_entsize = Int32.zero };
		(* shstrtab *)
		{ st_name = 56; st_type = StrTab; st_flags = 0;
			st_addr = Int32.zero; st_offset = 0; st_size = 0;
			st_link = 0; st_info = Int32.zero;
			st_addralign = 1; st_entsize = Int32.zero };
		|]
	
	let get_entry_point () =
		let sym = Hashtbl.find defined_symbols "__entrypoint" in
		let elf = List.assoc "libraries/kernel/stage1.o" !collected_objects in
		Vt100.printf "Entry point: value = %lx, size = %x, binding =  %x, type = %x, other = %x, section = %x\n"
			sym.sm_value sym.sm_size (st_bind sym.sm_info) (st_type sym.sm_info) sym.sm_other sym.sm_shndx;
		(* value holds a section offset for a defined symbol.
		   value is an offset from beginning of section.(shndx) *)
		let s = elf.header.section_headers.(sym.sm_shndx) in
		let s2 = Bitstring.string_of_bitstring
			(Bitstring.subbitstring elf.data
				(s.st_offset * 8) (s.st_size * 32))
		in
		Vt100.printf "%d: %s\n" (String.length s2) s2;
		let s3 = (String.sub s2 (Int32.to_int sym.sm_value) 4) in
		Vt100.printf "Address: %02x%02x%02x%02x\n"
			(Char.code s3.[0]) (Char.code s3.[1])
			(Char.code s3.[2]) (Char.code s3.[3]);
		0x0
	
	open IO
	
	let write_bytes os bytes =
		List.iter (write_byte os) bytes
	
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
		List.iter begin fun (n,elf) ->
			combine_sections sections elf.header
		end !collected_objects;
		Hashtbl.iter begin fun name list ->
			Vt100.printf "Section %-32s: %06x bytes\n" name (get_size list)
		end sections;
		(* let's just start hard-coding stuff, and see what we come up with *)
		let os = output_string () in
		(* ELF Header *)
		let foo = 0xbabe in
		write_bytes os [0x7f; Char.code 'E'; Char.code 'L'; Char.code 'F'];
		write_bytes os [0x01; 0x01; 0x01; 0x00; 0x00; 0x00; 0x00; 0x00; 0x00; 0x00; 0x00; 0x00; 0x00];
		write_ui16 os 2; (* executable *)
		write_ui16 os 3; (* i386 *)
		write_i32  os 1; (* version = 1 *)
		write_i32  os (get_entry_point ()); (* entry point would be address of __entrypoint *)
		write_i32  os 52; (* program header offset *)
		write_i32  os foo; (* section headers offset *)
		write_i32  os 0; (* no flags *)
		write_ui16 os 52; (* elf header size *)
		write_ui16 os 32; (* program header size *)
		write_ui16 os 2; (* number of program headers *)
		write_ui16 os 40; (* section header size *)
		write_ui16 os (Array.length section_headers);
		write_ui16 os (Array.length section_headers - 1);
		(* Program Headers *)
		write_i32  os 0x1; (* load *)
		write_i32  os 0x0; (* offset *)
		write_i32  os 0x00400000; (* virtual address *)
		write_i32  os 0x00400000; (* physical address *)
		write_i32  os foo; (* file size *)
		write_i32  os foo; (* memory size *)
		write_i32  os 0x7; (* read/write/execute *)
		write_i32  os 0x1000; (* alignment *)
		(* next header *)
		write_real_i32 os 0x6474E551l; (* GNU_STACK *)
		write_i32  os 0; (* rest is zero until end *)
		write_i32  os 0;
		write_i32  os 0;
		write_i32  os 0;
		write_i32  os 0;
		write_i32  os 0x7; (* read/write/execute *)
		write_i32  os 0x4; (* alignment, does it even matter? :P *)
		(*(* The actual program follows *)
		...;
		(* Then we have the section headers *)
		...;
		(* and our string table *)
		...;*)()

end
