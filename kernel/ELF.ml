
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
	section_name : int32 (* offset into string table *);
	section_type : section_type;
	section_flags : int32; (* 16 *)
	section_addr : int32;
	section_offset : int;
	section_size: int; (* 16 *)
	section_link: int32; (* 16 *)
	section_info: int32; (* 16 *)
	section_addralign: int32; (* 16 *)
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

let copy i = Int32.add Int32.zero i

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
		section_name = copy s_name;
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
		section_link = copy s_link;
		section_info = copy s_info;
		section_addralign = copy s_addralign;
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
		in {
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
		i (truncate 16 (get_string strtab (Int32.to_int h.section_name)))
		print_section_type h.section_type h.section_addr
		h.section_offset h.section_size

let print_type () = function
	| Relative -> "relocatable"
	| Executable -> "executable"
	| Shared -> "shared"

let print_header h =
	Vt100.printf "ELF Header:\n";
	Vt100.printf " Type: %a\n Entry point address: 0x%lx\n Start of program headers: %ld (bytes into file)\n Start of section headers: %ld (bytes into file)\n"
		print_type h.file_type h.entry h.phoff h.shoff;
	Vt100.printf " Flags: 0x%lx\n Size of this header: %d (bytes)\n Size of program headers: %d (bytes)\n Number of program headers: %d\n Size of section headers: %d (bytes)\n Number of sections headers: %d\n Section header string table index: %d\n"
		h.header_flags h.ehsize h.phentsize h.phnum h.shentsize h.shnum h.shstrndx;
	Vt100.printf "Section Headers:\n";
	Vt100.printf " [Nr] Name              Type           Addr     Off    Size   ES Flg Lk Inf Al\n";
	Array.iteri (print_section_header h.string_table) h.section_headers
