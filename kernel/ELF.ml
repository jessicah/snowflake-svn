
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
	(*section_headers : section_header list;*)
	shstrndx : int; (* 16 *)
}
and file_type
	= Relative (* 1 *)
	| Executable (* 2 *)
	| Shared (* 3 *)
and section_header = {
	section_name : int (* offset into string table *);
	section_type : section_type;
	section_flags : int; (* 16 *)
	section_addr : int32;
	section_offset : int32;
	section_size: int; (* 16 *)
	section_link: int; (* 16 *)
	section_info: int; (* 16 *)
	section_addralign: int; (* 16 *)
	section_entsize: int; (* 16 *)
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
	st_name : int32;
	st_value : int32;
	st_size: int32;
	st_info: char;
	st_other: char;
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
	} -> {
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
		(*section_headers : section_header list;*)
		shstrndx = shstrndx;
	}

let print_type () = function
	| Relative -> "relocatable"
	| Executable -> "executable"
	| Shared -> "shared"

let print_header h =
	Vt100.printf "ELF Header:\n";
	Vt100.printf " Type: %a\n Entry point address: 0x%lx\n Start of program headers: %ld (bytes into file)\n Start of section headers: %ld (bytes into file)\n"
		print_type h.file_type h.entry h.phoff h.shoff;
	Vt100.printf " Flags: 0x%lx\n Size of this header: %d (bytes)\n Size of program headers: %d (bytes)\n Number of program headers: %d\n Size of section headers: %d (bytes)\n Number of sections headers: %d\n Section header string table index: %d\n"
		h.header_flags h.ehsize h.phentsize h.phnum h.shentsize h.shnum h.shstrndx
