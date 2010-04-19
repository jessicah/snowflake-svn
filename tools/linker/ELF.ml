
module Defaults = struct
	(* we work with i386_elf only, so the sizes here are for that *)
	
	let sizeof_elf_header = 52
	let sizeof_program_header = 32
	let sizeof_section_header = 40
	
	(* using the default of ONE program header *)
	let sizeof_headers = sizeof_elf_header + sizeof_program_header
	
	let start_location = 0x08048000
	
	let entry_point = start_location + sizeof_headers
end

module T = struct
	type elf = {
		filename: string; (* instead of annoying tuple *)
		version : int;
		file_type : file_type;
		entry : int;
		phoff : int;
		shoff : int;
		header_flags : int;
		ehsize : int;
		phentsize : int;
		phnum : int;
		program_headers : program_header array;
		shentsize : int;
		shnum : int;
		section_headers : section_header array;
		shstrndx : int;
		strtab : string;
		data : Bitstring.bitstring; (* the raw file including parsed header *)
	} (* total: 52 bytes *)
	and file_type
		= Relative | Executable | Shared
	and section_header = {
		st_name : int;
		st_type : section_type;
		st_flags : section_flag list;
		st_addr : int;
		st_offset : int;
		st_size : int;
		st_link : int;
		st_info : int;
		st_addralign : int;
		st_entsize : int;
	}
	and section_type
		= Null | Progbits | Symtab | Strtab | Rela
		| Hash | Dynamic | Note | Nobits | Rel
		| Shlib | Dynsym | Other of int
	and section_flag
		= Write | Alloc | Execute (* don't care about other types *)
	and program_header = {
		p_type : int; (* 4 *)
		p_offset : int; (* 4 *)
		p_vaddr : int; (* 4 *)
		p_paddr : int; (* 4 *)
		p_filesz : int; (* 4 *)
		p_memsz : int; (* 4 *)
		p_flags : int; (* 4 *) (* 0x4 PF_R, 0x2 PF_W, 0x1 PF_X *)
		p_align: int; (* 4 *)
	} (* total: 32 bytes *)
	
	(*
		STB_LOCAL   = 0
		STB_GLOBAL  = 1
		STB_WEAK    = 2
	
		STT_NOTYPE  = 0
		STT_OBJECT  = 1
		STT_FUNC    = 2
		STT_SECTION = 3
		STT_FILE    = 4
		STT_COMMON  = 5
		STT_TLS     = 6
		
		ST_BIND(x) = ((x) >> 4)
		ST_TYPE(x) = (((unsigned int) x) & 0xf)
	*)
	
	type symtab_entry = {
		sm_name : int;
		sm_value : int;
		sm_size : int;
		sm_info : int;
		sm_other : int;
		sm_shndx : int;
	}
	
	(* R_SYM(x) = ((x) >> 8), R_TYPE(x) = ((x) & 0xff) *)
	
	type rel = {
		r_offset : int; (* 4 *)
		r_info : int;   (* 4 *)
	}
	
	type rela = {
		ra_offset : int;
		ra_info : int;
		ra_addend : int;
	}
	
	type t
		= Object of elf
		| Archive of elf list
end

module O = struct
	open T
	open IO
	
	let output_program_header io program_header =
		write_i32 io program_header.p_type;
		write_i32 io program_header.p_offset;
		write_i32 io program_header.p_vaddr;
		write_i32 io program_header.p_paddr;
		write_i32 io program_header.p_filesz;
		write_i32 io program_header.p_memsz;
		write_i32 io program_header.p_flags;
		write_i32 io program_header.p_align
	
	let section_type_to_int = function
		| Other x -> x
		| t -> Obj.magic t
	
	let section_flags_to_int flags =
		let masks = [ Write, 0x1; Alloc, 0x2; Execute, 0x4 ] in
		let flags = List.filter begin fun p ->
				List.exists (fun flag -> fst p = flag) flags
			end masks in
		List.fold_left (+) 0 (List.map snd flags)
	
	let output_section_header io section_header =
		write_i32 io section_header.st_name;
		write_i32 io (section_type_to_int section_header.st_type);
		write_i32 io (section_flags_to_int section_header.st_flags);
		write_i32 io section_header.st_addr;
		write_i32 io section_header.st_offset;
		write_i32 io section_header.st_size;
		write_i32 io section_header.st_link;
		write_i32 io section_header.st_info;
		write_i32 io section_header.st_addralign;
		write_i32 io section_header.st_entsize
	
	let output oc entry_point program_header content =
		(* output an ELF header, followed by one program header,
			followed by the content, and be done with it *)
		let io = output_channel oc in
		(* ELF header *)
		let shstrtab = "\000.text\000.shstrtab\000" in
		let section_headers = [
			{ (* NULL *)
				st_name      = 0;
				st_type      = Null;
				st_flags     = [];
				st_addr      = 0;
				st_offset    = 0;
				st_size      = 0;
				st_link      = 0;
				st_info      = 0;
				st_addralign = 0;
				st_entsize   = 0;
			};
			{ (* .text *)
				st_name      = 1;
				st_type      = Progbits;
				st_flags     = [Alloc; Execute];
				st_addr      = entry_point;
				st_offset    = Defaults.sizeof_headers;
				st_size      = String.length content;
				st_link      = 0;
				st_info      = 0;
				st_addralign = 4; (* word size *)
				st_entsize   = 0;
			};
			{ (* .shstrtab *)
				st_name      = 7;
				st_type      = Strtab;
				st_flags     = [];
				st_addr      = 0;
				st_offset    = Defaults.sizeof_headers + String.length content;
				st_size      = String.length shstrtab;
				st_link      = 0;
				st_info      = 0;
				st_addralign = 1; (* byte aligned *)
				st_entsize   = 0;
			};
		] in
		let header = {
			(* we don't store machine atm, we only deal with i386 :P *)
			filename = "a.out";
			version = 1;
			file_type = Executable;
			entry = entry_point;
			(* Program Headers follow the ELF Header *)
			phoff = Defaults.sizeof_elf_header;
			(* Section Headers follow the ELF Header, Program Headers, sections, and shstrtab *)
			shoff = Defaults.sizeof_headers + String.length content + 20;
			header_flags = 0;
			ehsize = Defaults.sizeof_elf_header;
			phentsize = Defaults.sizeof_program_header;
			phnum = 1;
			program_headers = [| program_header |];
			shentsize = Defaults.sizeof_section_header;
			shnum = List.length section_headers;
			section_headers = Array.of_list section_headers;
			shstrndx = 2;
			strtab = shstrtab;
			data = Bitstring.empty_bitstring;
		} in
		(* magic 1 1 1 rest are 0s *)
		nwrite io "\x7FELF\x01\x01\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00";
		write_i16 io 2; (* type: executable *)
		write_i16 io 3; (* machine:386 *)
		write_i32 io header.version;
		write_i32 io header.entry;
		write_i32 io header.phoff;
		(* 20 is 17 (size of shstrtab rounded up to nearest word boundary *)
		write_i32 io header.shoff;
		write_i32 io header.header_flags;
		write_i16 io header.ehsize;
		write_i16 io header.phentsize;
		write_i16 io header.phnum;
		write_i16 io header.shentsize;
		write_i16 io header.shnum;
		write_i16 io header.shstrndx;
		(* program header *)
		output_program_header io header.program_headers.(0);
		(* content *)
		nwrite io content;
		(* shstrtab *)
		nwrite io header.strtab; (* length = 17 *)
		nwrite io "\000\000\000"; (* padding for word alignment *)
		(* section headers *)
		Array.iter (output_section_header io) header.section_headers;
		flush io
		
end

module P = struct
	open T
	
	let copy i = Int32.add Int32.zero i
	let to_int = Int32.to_int
	
	(*** PARSING SYMTAB ***)
	
	let rec parse_symtab acc bits =
		if Bitstring.bitstring_length bits = 0 then
			Array.of_list (List.rev acc)
		else
			bitmatch bits with
			| { name  : 32 : littleendian;
				value : 32 : littleendian;
				size  : 32 : littleendian;
				info  : 8 : littleendian;
				other : 8 : littleendian;
				shndx : 16 : littleendian;
				rest  : -1 : bitstring } ->
			parse_symtab ({
				sm_name = to_int name;
				sm_value = to_int value;
				sm_size = to_int size;
				sm_info = info;
				sm_other = other;
				sm_shndx = shndx } :: acc) rest
	
	let parse_symtab bits = parse_symtab [] bits
	
	(*** PARSING SECTION HEADERS ***)
	
	let parse_flags flags =
		let masks = [ Write, 0x01; Alloc, 0x02; Execute, 0x04 ] in
		let flags = List.filter begin fun pair ->
				snd pair land flags <> 0
			end masks in
		List.map fst flags
	
	let parse_section_header bits =
		bitmatch bits with
		| { s_name      : 32 : littleendian;
			s_type      : 32 : littleendian;
			s_flags     : 32 : littleendian;
			s_addr      : 32 : littleendian;
			s_offset    : 32 : littleendian;
			s_size      : 32 : littleendian;
			s_link      : 32 : littleendian;
			s_info      : 32 : littleendian;
			s_addralign : 32 : littleendian;
			s_entsize   : 32 : littleendian } ->
		{
			st_name = to_int s_name;
			st_type =
				begin let n = to_int s_type in
					if n < 12 then Obj.magic n else Other n
				end;
			st_flags = parse_flags (to_int s_flags);
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
			| { data : size * 8 : bitstring; rest : -1 : bitstring } ->
				parse_section_headers
					(parse_section_header data :: acc) (num - 1) size rest
			| { _ : -1 : bitstring } -> Array.of_list (List.rev acc)
	
	(*** PARSING ELF HEADER ***)
	
	let parse_elf bits filename =
		bitmatch bits with
		| { "\x7FELF\x01\x01" : 6 * 8 : string;
			_ : 10 * 8 : bitstring;
			elf_type : 16 : littleendian;
			machine : 16 : littleendian;
			version : 32 : littleendian;
			entry : 32 : littleendian;
			phoff : 32 : littleendian;
			shoff : 32 : littleendian;
			flags : 32 : littleendian;
			ehsize : 16 : littleendian;
			phentsize : 16 : littleendian;
			phnum : 16 : littleendian;
			shentsize : 16 : littleendian;
			shnum : 16 : littleendian;
			shstrndx : 16 : littleendian } ->
		let section_headers =
			parse_section_headers [] shnum shentsize
				(Bitstring.subbitstring bits
					(to_int shoff * 8)
					(shentsize * shnum * 8))
		in
		let strtab =
			Bitstring.string_of_bitstring
				(Bitstring.subbitstring bits
					(section_headers.(shstrndx).st_offset * 8)
					(section_headers.(shstrndx).st_size * 8))
		in
		{
			filename = filename;
			version = to_int version;
			file_type =
				begin match elf_type with
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
			program_headers = [| |]; (* objects don't have program headers *)
			shentsize = shentsize;
			shnum = shnum;
			section_headers = section_headers;
			shstrndx = shstrndx;
			strtab = strtab;
			data = bits;
		}
	| { _ : -1 : bitstring } -> failwith "Not an ELF file"
	
	(*** PARSING ARCHIVES ***)
	
	let index_from s o c =
		try Some (String.index_from s o c)
		with Not_found -> None
	
	let rec build_lookup_table ht s off len =
		if off >= len then ()
		else
			match index_from s off '\n' with
			| Some i ->
				if i = off then ()
				else begin
					let sub = String.sub s off (i-1-off) in
					Hashtbl.add ht off (sub ^ "/");
					build_lookup_table ht s (i+1) len
				end
			| None -> ()
	
	let build_lookup_table ht s =
		build_lookup_table ht s 0 (String.length s)
	
	let fix_name name =
		String.sub name 0 (String.index name '/')
	
	let rec parse_archive_members acc ht bits =
		if Bitstring.bitstring_length bits = 0 then acc
		else
			bitmatch bits with
			| { name : 16 * 8 : string;
				_ : 12 * 8 : bitstring; (* date *)
				_ : 6 * 8 : bitstring; (* uid *)
				_ : 6 * 8 : bitstring; (* gid *)
				_ : 8 * 8 : bitstring; (* mode *)
				size : 10 * 8 : string;
				0x600A : 2 * 8;
				filedata :
					(Scanf.sscanf size "%d" (fun x -> x)) * 8 : bitstring;
				rest : -1 : bitstring } ->
			begin match name with
			| "/               " ->
				parse_archive_members acc ht rest
			| "//              " ->
				(* build the lookup table *)
				build_lookup_table ht
					(Bitstring.string_of_bitstring filedata);
				parse_archive_members acc ht rest
			| name when name.[0] = '/' ->
				(* need to do a lookup *)
				let name = fix_name
					(Hashtbl.find ht
						(Scanf.sscanf name "/%d" (fun x -> x)))
				in
				begin try
					let member = (parse_elf filedata name) in
					parse_archive_members (member :: acc) ht rest
				with Failure _ ->
					parse_archive_members acc ht rest
				end
			| name ->
				begin try
					let member = (parse_elf filedata (fix_name name)) in
					parse_archive_members (member :: acc) ht rest
				with Failure _ ->
					parse_archive_members acc ht rest
				end
			end
		| { 0x0A : 8; rest : -1 : bitstring } ->
			parse_archive_members acc ht rest
	
	let parse_archive bits =
		bitmatch bits with
		| { "!<arch>\n" : 8 * 8 : string;
			rest : -1 : bitstring } ->
				parse_archive_members [] (Hashtbl.create 7) rest
		| { _ : -1 : bitstring } -> failwith "Not an archive"
	
	let parse filename =
		let ic = open_in_bin filename in
		let len = in_channel_length ic in
		let buf = String.create len in
		really_input ic buf 0 len;
		let bits = Bitstring.bitstring_of_string buf in
		begin
			try
				Object (parse_elf bits filename)
			with Failure _ -> begin
			try
				Archive (parse_archive bits)
			with Failure _ ->
				failwith "Unable to parse as ELF or Archive"
			end
		end
end

module Printing = struct
	open T
	
	let print_section_type oc = function
		| Null -> Printf.fprintf oc "%-12s" "NULL"
		| Progbits -> Printf.fprintf oc "%-12s" "PROGBITS"
		| Symtab -> Printf.fprintf oc "%-12s" "SYMTAB"
		| Strtab -> Printf.fprintf oc "%-12s" "STRTAB"
		| Rela -> Printf.fprintf oc "%-12s" "RELA"
		| Hash -> Printf.fprintf oc "%-12s" "HASH"
		| Dynamic -> Printf.fprintf oc "%-12s" "DYNAMIC"
		| Note -> Printf.fprintf oc "%-12s" "NOTE"
		| Nobits -> Printf.fprintf oc "%-12s" "NOBITS"
		| Rel -> Printf.fprintf oc "%-12s" "REL"
		| Shlib -> Printf.fprintf oc "%-12s" "SHLIB"
		| Dynsym -> Printf.fprintf oc "%-12s" "DYNSYM"
		| Other x -> Printf.fprintf oc "unknown <%d>" x

	let get_string strtab off =
		String.sub strtab off (String.index_from strtab off '\000' - off)

	let truncate len s =
		if String.length s <= len then s
		else String.sub s 0 len

	let flags_to_string flags =
		let strings = List.map begin function
			| Write -> "W"
			| Alloc -> "A"
			| Execute -> "X"
		end flags in
		String.concat "" strings

	let print_section_header strtab i h =
		Printf.printf "  [%2d] %-16s  %a    %08x %06x %06x %02x %3s %2d %3d %2d\n"
			i (truncate 16 (get_string strtab h.st_name))
			print_section_type h.st_type h.st_addr
			h.st_offset h.st_size
			h.st_entsize (flags_to_string h.st_flags) h.st_link h.st_info h.st_addralign

	let print_type oc = function
		| Relative -> Printf.fprintf oc "relocatable"
		| Executable -> Printf.fprintf oc "executable"
		| Shared -> Printf.fprintf oc "shared"

	let print_header h =
		Printf.printf "ELF Header:\n";
		Printf.printf "  Type: %a\n Entry point address: 0x%x\n Start of program headers: %d (bytes into file)\n Start of section headers: %d (bytes into file)\n"
			print_type h.file_type h.entry h.phoff h.shoff;
		Printf.printf " Flags: 0x%x\n Size of this header: %d (bytes)\n Size of program headers: %d (bytes)\n Number of program headers: %d\n Size of section headers: %d (bytes)\n Number of sections headers: %d\n Section header string table index: %d\n"
			h.header_flags h.ehsize h.phentsize h.phnum h.shentsize h.shnum h.shstrndx;
		Printf.printf "There are %d section headers, starting at offset 0x%x:\n\n"
			(Array.length h.section_headers) h.shoff;
		Printf.printf "Section Headers:\n";
		Printf.printf "  [Nr] Name              Type            Addr     Off    Size   ES Flg Lk Inf Al\n";
		Array.iteri (print_section_header h.strtab) h.section_headers


	let print = function
		| Object elf ->
			Printf.printf "Object: %s\n" elf.filename;
			print_header elf
		| Archive list ->
			Printf.printf "Archive:\n";
			List.iter (fun elf ->
				Printf.printf "Member: %s\n" elf.filename;
				print_header elf) list
end

module L = struct
	type obj = {
		header : T.elf;
		sections : section array;
		symtab : T.symtab_entry array;
		strtab : string;
	}
	and section = {
		header : T.section_header;
		name : string;
		padding : int;
	}
end

open T

let open_files file_list =
	let o, a = List.partition (function Object _ -> true | _ -> false)
			(List.map P.parse file_list)
	in
		List.map (function Object o -> o | _ -> assert false) o,
		List.map (function Archive a -> a | _ -> assert false) a

let objects, libraries = open_files (List.tl (Array.to_list Sys.argv))

let get_section obj name =
	List.find begin function section ->
			Printing.get_string obj.strtab section.st_name = name
		end (Array.to_list obj.section_headers)


(* builds a string table *)
let names = [ ""; ".text"; ".shstrtab" ]

let build_string_table names =
	let len = List.fold_left begin fun acc name ->
			acc + String.length name + 1
		end 0 names in
	let buf = String.make len '\000' in
	let indices =
		List.fold_left begin fun ixs name ->
			let len = String.length name in
			let ix = List.hd ixs in
			String.blit name 0 buf ix len;
			ix + len + 1 :: ixs
		end [0] names
	in
	(* strtab, indices, length *)
	buf, List.rev (List.tl indices), List.hd indices

let align4 x = ((x + 3) lsr 2) lsl 2

(* cheating, but already know .data and .bss are empty, so won't get output *)
let () =
	(* result of contents is reverse order to objects on command line *)
	let contents = List.fold_left begin fun acc obj ->
			let text = get_section obj ".text" in
			let aligned_size = align4 text.st_size in 
			let buf = String.make aligned_size '\x90' in
			let (filedata, _, _) = obj.data in
			String.blit filedata text.st_offset buf 0 text.st_size;
			(buf, text.st_size) :: acc
	end [] objects in
	(* use the last one to truncate to actual size (no padding for last one) *)
	let last = List.hd contents in
	let size = snd last + List.fold_left begin fun sz (str,_) ->
			sz + String.length str
		end 0 (List.tl contents) in
	(* then concatenate and truncate *)
	let contents = String.concat "" (List.rev_map fst contents) in
	let contents = String.sub contents 0 size in
	let ph = {
		p_type = 1; (* LOAD *)
		p_offset = 0; (* I dunno what that's for *)
		p_vaddr = Defaults.start_location;
		p_paddr = Defaults.start_location;
		p_filesz = size + Defaults.sizeof_headers;
		p_memsz = size + Defaults.sizeof_headers;
		p_flags = 5; (* read | execute *)
		p_align = 0x1000;
	} in
	let oc = open_out_bin "a.out" in
	O.output oc Defaults.entry_point ph contents;
	close_out oc

(*

BASICALLY HOW TO LINK snowflake.native FROM OUR LDSCRIPT

location start (a.k.a. offset) = 0x00400000 + SIZEOF_HEADERS (say 0x100)

.mb_header : { *(.mb_header) LONG(0) } (* add 0x00000000 after .mb_header *)

(* skip dynamic stuff for now... *)

.text : { *(.text .text.* .gnu.linkonce.t.* ) }
PROVIDE (_etext = .); (* define symbol _etext if not defined *)
.rodata : { *(.rodata .rodata.* .gnu.linkonce.r.* ) }
.data : { *(.data .data.* .gnu.linkonce.d.* ) }
_edata = .; PROVIDE (edata = .); (* define symbol edata if not defined *)
. = ALIGN(0x1000); (* updates location *)
__bss_start = .;
.bss : { *(.bss.pagealigned) *(.bss .bss.* .gnu.linkonce.b.* ) *(COMMON) }
_end = .; PROVIDE (end = .); (* define symbol end if not defined *)

(* skip stabs debugging sections *)

(* and also dwarf stuff *)

*)
