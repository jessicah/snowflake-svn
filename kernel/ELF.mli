

(* ELF -- The Executable and Linking Format *)

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

val parse_elf_header : Bitstring.t -> header

val print_header : header -> unit
