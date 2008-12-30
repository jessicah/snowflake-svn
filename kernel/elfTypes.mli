
type header = {
	version : int;
	file_type : file_type;
	entry : int;
	phoff : int;
	shoff : int;
	header_flags : int; (* 16 *)
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
	= Relative | Executable | Shared (* Obj.magic (n-1) *)
and section_header = {
	st_name : int (* offset into string table *);
	st_type : section_type;
	st_flags : int; (* 16 *)
	st_addr : int;
	st_offset : int;
	st_size: int; (* 16 *)
	st_link: int; (* 16 *)
	st_info: int; (* 16 *)
	st_addralign: int; (* 16 *)
	st_entsize: int; (* 16 *)
}
and section_type (* if n < 12 then Obj.magic n *)
	= Null | ProgBits | SymTab | StrTab | RelA
	| Hash | Dynamic | Note | NoBits | Rel
	| ShLib | Dynsym | Other of int

type elf = { header : header; data : Bitstring.t }

type t
	= Object of elf
	| Archive of (string * elf) list

type symbol_table_entry = {
	sm_name : int;
	sm_value : int; (* might need to keep as int32... *)
	sm_size: int;
	sm_info: int;
	sm_other: int;
	sm_shndx: int;
}

type rel = { (* these might need to be int32... *)
	r_offset: int;
	r_info: int;
}

type rela = { (* these might need to be int32 too... *)
	ra_offset: int;
	ra_info: int;
	ra_addend: int; (* signed *)
}

exception Not_elf_file
exception Not_archive
exception Not_elf_or_archive_file
