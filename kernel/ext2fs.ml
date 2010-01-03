
(* An ext2 file system driver... need somewhere to start with this whole file system
	stuff, the eventual virtual file system design, and all that cruft... *)

open Partitions
open IO

(* note: order of fields is reversed compared to order on disk *)
type superblock = {
	(* other options *)
	reserved_3 : int list;
	s_first_meta_bg : int32;
	s_default_mount_options : int32;
	(* directory indexing support *)
	reserved_2 : int list;
	s_def_hash_version : int;
	hash_seed : int list;
	(* journalling support *)
	s_last_orphan : int32;
	s_journal_dev : int32;
	s_journal_inum : int32;
	s_journal_uuid : int list;
	(* performance hints *)
	reserved_1 : int list;
	s_prealloc_dir_blocks : int;
	s_prealloc_blocks : int;
	(* ext2_dynamic_rev specific... *)
	s_algo_bitmap : int32;
	s_last_mounted : int list;
	s_volume_name : int list;
	s_uuid : int list;
	s_feature_ro_compat : int32;
	s_feature_incompat : int32;
	s_feature_compat : int32;
	s_block_group_nr : int;
	s_inode_size : int;
	s_first_ino : int;
	(* typical stuff :) *)
	s_def_resgid : int;
	s_def_resuid : int;
	s_rev_level : int;
	s_creator_os : int32;
	s_checkinterval : int;
	s_lastcheck : int32;
	s_minor_rev_level : int;
	s_errors : int;
	s_state : int;
	s_magic : int;
	s_max_mnt_count : int;
	s_mnt_count : int;
	s_wtime : int32;
	s_mtime : int32;
	s_inodes_per_group : int;
	s_frags_per_group : int;
	s_blocks_per_group : int;
	s_log_frag_size : int;
	s_log_block_size : int;
	s_first_data_block : int;
	s_free_inodes_count : int;
	s_free_blocks_count : int;
	s_r_blocks_count : int;
	s_blocks_count : int;
	s_inodes_count : int;
}

type block_group_descriptor = {
	bg_reserved : int list;
	bg_pad : int;
	bg_used_dirs_count : int;
	bg_free_inodes_count : int;
	bg_free_blocks_count :  int;
	bg_inode_table : int;
	bg_inode_bitmap : int;
	bg_block_bitmap : int;
}

type i_mode = i_format * i_rights
and i_rights = int
and i_format
= Socket
| Symlink
| File
| Block_device
| Directory
| Char_device
| FIFO
| Unknown

type inode = {
	i_osd2 : int list;
	i_faddr : int32;
	i_dir_acl : int32;
	i_file_acl : int32;
	i_generation : int32;
	i_block : int array;
	i_osd1 : int32;
	i_flags : int32;
	i_blocks : int;
	i_links_count : int;
	i_gid : int;
	i_dtime : int32;
	i_mtime : int32;
	i_ctime : int32;
	i_atime : int32;
	i_size : int;
	i_uid : int;
	i_mode : i_mode;
}

type dir_entry = {
	name : string;
	file_type : int; (* i_format *)
	name_len : int;
	rec_len : int;
	inode : int32;
}

type t = {
	p : Partitions.partition_t;
	s : superblock;
	t : block_group_descriptor array;
	r : inode;
	bs : int;
}

type fs = {
	metadata : t;
	read_dir : inode -> dir_entry list;
	read_inode : int32 -> inode;
	read_file : inode -> string;
	read_file_ba : inode -> (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t;
}

(* IO helper function *)

let read_bytes i n =
	let rec read acc = function
		| 0 -> List.rev acc
		| n -> read (IO.read_byte i :: acc) (n-1)
	in read [] n

let read_asciz i =
	let rec read acc = function
		| 0 -> ExtString.String.implode (List.rev_map Char.chr acc)
		| c -> read (c :: acc) (IO.read_byte i)
	in read [] (IO.read_byte i)

let to_sector s block =
	block lsl (s.s_log_block_size + 1)

(* the addition ensures we read enough of a sector to get all the data *)
let to_num_sectors n = (n + 511) / 512

(* read a superblock *)
let superblock partition =
	if partition.info.code <> 0x83 then failwith "not linux native partition";
	let superblock_raw = partition.read 2 2 in
	let i = IO.input_string superblock_raw in
	{
		s_inodes_count = read_i32 i;
		s_blocks_count = read_i32 i;
		s_r_blocks_count = read_i32 i;
		s_free_blocks_count = read_i32 i;
		s_free_inodes_count = read_i32 i;
		s_first_data_block = read_i32 i;
		s_log_block_size = read_i32 i;
		s_log_frag_size = read_i32 i;
		s_blocks_per_group = read_i32 i;
		s_frags_per_group = read_i32 i;
		s_inodes_per_group = read_i32 i;
		s_mtime = read_real_i32 i;
		s_wtime = read_real_i32 i;
		s_mnt_count = read_i16 i;
		s_max_mnt_count = read_i16 i;
		s_magic = read_i16 i;
		s_state = read_i16 i;
		s_errors = read_i16 i;
		s_minor_rev_level = read_i16 i;
		s_lastcheck = read_real_i32 i;
		s_checkinterval = read_i32 i;
		s_creator_os = read_real_i32 i;
		s_rev_level = read_i32 i;
		s_def_resuid = read_i16 i;
		s_def_resgid = read_i16 i;
		(* ext2_dynamic_rev specific... *)
		s_first_ino = read_i32 i;
		s_inode_size = read_i16 i;
		s_block_group_nr = read_i16 i;
		s_feature_compat = read_real_i32 i;
		s_feature_incompat = read_real_i32 i;
		s_feature_ro_compat = read_real_i32 i;
		s_uuid = read_bytes i 16;
		s_volume_name = read_bytes i 16;
		s_last_mounted = read_bytes i 64;
		s_algo_bitmap = read_real_i32 i;
		(* performance hints *)
		s_prealloc_blocks = read_byte i;
		s_prealloc_dir_blocks = read_byte i;
		reserved_1 = read_bytes i 2;
		(* journalling support *)
		s_journal_uuid = read_bytes i 16;
		s_journal_inum = read_real_i32 i;
		s_journal_dev = read_real_i32 i;
		s_last_orphan = read_real_i32 i;
		(* directory indexing support *)
		hash_seed = read_bytes i 16;
		s_def_hash_version = read_byte i;
		reserved_2 = read_bytes i 3;
		(* other options *)
		s_default_mount_options = read_real_i32 i;
		s_first_meta_bg = read_real_i32 i;
		reserved_3 = read_bytes i 760;
	}

let block_group_descriptor_table p s =
	(* located on the first block following the superblock: 1 or 2 *)
	let sector_start =
		if s.s_log_block_size = 0 then
			(* block size is 1024, superblock at block 1 *)
			to_sector s 2
		else
			(* block size is > 1024, superblock inside block 0 *)
			to_sector s 1
	in
	(* num groups gives us the number of entries *)
	let num_groups = s.s_inodes_count / s.s_inodes_per_group in
	let i,o = IO.pipe () in
	(* Array.init works in the expected order *)
	Array.init num_groups begin fun group ->
		if group mod 16 = 0 then
			IO.nwrite o (p.read (sector_start + (group / 16)) 1);
		(*let i = IO.input_string (p.read (sector_start + group) 1) in*)
		{
			bg_block_bitmap = read_i32 i;
			bg_inode_bitmap = read_i32 i;
			bg_inode_table = read_i32 i;
			bg_free_blocks_count = read_i16 i;
			bg_free_inodes_count = read_i16 i;
			bg_used_dirs_count = read_i16 i;
			bg_pad = read_i16 i;
			bg_reserved = read_bytes i 12;
		}
	end

let to_imode x =
	let format = match x land 0xF000 with
		| 0xC000 -> Socket
		| 0xA000 -> Symlink
		| 0x8000 -> File
		| 0x6000 -> Block_device
		| 0x4000 -> Directory
		| 0x2000 -> Char_device
		| 0x1000 -> FIFO
		| _ -> Unknown
	in
	format, x land 0x1FF

let of_imode (format, rights) =
	let f = match format with
		| Socket -> "socket"
		| Symlink -> "symlink"
		| File -> "file"
		| Block_device -> "block device"
		| Directory -> "directory"
		| Char_device -> "char device"
		| FIFO -> "fifo"
		| Unknown -> "<unknown>"
	in
	Printf.sprintf "kind: %s, rights: %03o" f rights

let null_inode = {
		i_mode = Unknown, 0;
		i_uid = 0;
		i_size = 0;
		i_atime = 0l;
		i_ctime = 0l;
		i_mtime = 0l;
		i_dtime = 0l;
		i_gid = 0;
		i_links_count = 0;
		i_blocks = 0;
		i_flags = 0l;
		i_osd1 = 0l;
		i_block = Array.make 15 0;
		i_generation = 0l;
		i_file_acl = 0l;
		i_dir_acl = 0l;
		i_faddr = 0l;
		i_osd2 = [];
	}

let inode fs x =
	let x = Int32.to_int x in (* maybe it doesn't have to be an int32 afterall... *)
	let block_group = (x - 1) / fs.s.s_inodes_per_group in
	let inode_index = (x - 1) mod fs.s.s_inodes_per_group in
	let descr = fs.t.(block_group) in
	(*Vt100.printf "#%ld, group %d, index %d, inodes per group %d, inode_table %d\n"
		z block_group inode_index s.s_inodes_per_group descr.bg_inode_table;*)
	let inode_size =
		if fs.s.s_rev_level >= 1 (* ext2_dynamic_rev *)
		then fs.s.s_inode_size
		else 128 in
	(* bg_inode_table is the first block of the inode table *)
	let sector = descr.bg_inode_table lsl (fs.s.s_log_block_size + 1) in
	let sector = sector + ((inode_index * inode_size) / 512) in
	(*Vt100.printf "inode %d located at offset %d, block group = %d, inode index = %d\n" x sector block_group inode_index;
	Vt100.printf "inode table = %x, block size = %d (log = %d), inode_size = %d\n" descr.bg_inode_table (1024 lsl s.s_log_block_size) s.s_log_block_size inode_size;*)
	(* an inode always fits within a sector, so it works okay *)
	let sector = fs.p.read sector 1 in
	let offset = (inode_index * inode_size) mod 512 in
	let i = IO.input_string sector in
	(*let read_i32 x = Vt100.printf "x"; read_i32 x in*)
	ignore (read_bytes i offset);
	{
		i_mode = to_imode (read_i16 i);
		i_uid = read_i16 i;
		i_size = read_i32 i;
		i_atime = read_real_i32 i;
		i_ctime = read_real_i32 i;
		i_mtime = read_real_i32 i;
		i_dtime = read_real_i32 i;
		i_gid = read_i16 i;
		i_links_count = read_i16 i;
		i_blocks = read_i32 i;
		i_flags = read_real_i32 i;
		i_osd1 = read_real_i32 i;
		i_block = Array.init 15 (fun _ -> read_i32 i (*let x = read_real_i32 i in Vt100.printf "%ld\n" x; Int32.to_int x*)); (* ls cdrom: fails here *)
		i_generation = read_real_i32 i;
		i_file_acl = read_real_i32 i;
		i_dir_acl = read_real_i32 i;
		i_faddr = read_real_i32 i;
		i_osd2 = read_bytes i 12;
	}

let readdir fs inode =
	let block_size = 1024 lsl fs.s.s_log_block_size in (* not using this as yet *)
	(*let inode_size = ref inode.i_size in*)
	(*Vt100.printf "block size: %d, inode size: %d, iterations: %d\n"
		block_size !inode_size (!inode_size / block_size);*)
	(* copy the data into the buffer *)
	let buffer = fs.p.read (to_sector fs.s inode.i_block.(0)) (2 lsl fs.s.s_log_block_size) in
	let i, o = IO.pipe () in
	(*let n = ref 1 in (* unused *)*)
	(* fill buffer *)
	IO.nwrite o buffer;
	let total_length = ref 0 in
	let rec loop acc =
		let entry = {
			inode = read_real_i32 i;
			rec_len = read_i16 i;
			name_len = read_byte i;
			file_type = read_byte i;
			name = "";
		} in
		total_length := !total_length + entry.rec_len;
		let entry = { entry with name = ExtString.String.implode (List.map Char.chr (read_bytes i entry.name_len)) } in
		(*Vt100.printf "dir_entry: inode = %ld, rec_len = %d, name_len = %d, file_type = %d, name = %s\n"
			entry.inode entry.rec_len entry.name_len entry.file_type entry.name;*)
		if !total_length >= block_size then begin
			(* this record is pointing to the next block *)
			(*if inode.i_block.(!n) = 0 then begin
				(* we're finished *)
				List.rev (entry :: acc)
			end else begin
				(* we have another block to read *)
				total_length := 0;
				ignore (IO.read_all i); (* consume remaining input we can discard *)
				IO.write_string o (p.read (to_sector s inode.i_block.(!n)) (2 lsl s.s_log_block_size));
				incr n;
				loop (entry :: acc)
			end*)List.rev (entry :: acc)
		end else begin
			(* need to actually read the padding... *)
			(*Vt100.printf "skipping %dbytes of padding...\n"
				(entry.rec_len - 8 - String.length entry.name);*)
			ignore (read_bytes i (entry.rec_len - 8 - String.length entry.name));
			loop (entry :: acc)
		end
	in loop []

let readfile fs inode =
	(* just read the first 8 blocks *)
	let indirect_entries = (1024 lsl fs.s.s_log_block_size) / 4 in
	let num_sectors = 2 lsl fs.s.s_log_block_size in
	let read ofs = fs.p.read (to_sector fs.s ofs) num_sectors in
	let o = IO.output_string () in
	for i = 0 to 11 do
		if inode.i_block.(i) <> 0 then begin
			(* we have some data! *)
			IO.nwrite o (read inode.i_block.(i));
		end;
	done;
	if inode.i_block.(12) <> 0 then begin
		(* need to start reading data from indirect blocks... *)
		let i2 = IO.input_string (read inode.i_block.(12)) in
		for i = 0 to (1024 lsl fs.s.s_log_block_size) / 4 - 1 do
			let x = IO.read_i32 i2 in
			if x <> 0 then begin
				IO.nwrite o (read x);
			end
		done;
	end;
	if inode.i_block.(13) <> 0 then begin
		(* need to read doubly-indirect blocks... fun... *)
		let i3 = IO.input_string (read inode.i_block.(13)) in
		(* just do first one *)
		for i = 0 to indirect_entries / 8 - 1 do
			let x = IO.read_i32 i3 in
			if x <> 0 then begin
				let i4 = IO.input_string (read x) in
				for i = 0 to indirect_entries - 1 do
					let y = IO.read_i32 i4 in
					if y <> 0 then begin
						IO.nwrite o (read y);
					end;
				done;
			end;
		done;
	end;
	let s = IO.close_out o in
	Vt100.printf "ext2fs: read %d bytes from disk\n" (String.length s);
	String.sub s 0 (min inode.i_size (String.length s))

let loop_until limit f =
	let rec loop n =
		if n = limit then
			()
		else begin
			if f n then loop (n+1) else ()
		end
	in loop 0

type blockIndex
	= Direct of int
	| Indirect of int
	| DoubleIndirect of int * int
	| TripleIndirect of int * int * int

let divmod x y = (x / y, x mod y)

let read_file_range_ba fs inode ofs len =
	(* trim len to inode.i_size *)
	let len = min (inode.i_size - ofs) len in
	(* some "constants", should be already calculated .... *)
	let indirect_entries = (1024 lsl fs.s.s_log_block_size) / 4 in
	let num_sectors = 2 lsl fs.s.s_log_block_size in
	let block_size = 1024 lsl fs.s.s_log_block_size in
	(* read a "block" from disk *)
	let read ofs = fs.p.read (to_sector fs.s ofs) num_sectors in
	(* the bigarray "buffer", note padding added; we'll extract sub-array at the end *)
	let ba = Bigarray.Array1.create Bigarray.int8_unsigned Bigarray.c_layout (len + block_size + block_size) in
	(* writing function and bookkeeping *)
	let pos = ref 0 in
	let write ofs =
		(* get the disk data *)
		let data = read ofs in
		let len = if inode.i_size - !pos < block_size
			then inode.i_size - !pos
			else block_size
		in
		begin try
			Bigarray.Array1.blit_from_string
				(if len = block_size then data else String.sub data 0 len)
				(Bigarray.Array1.sub ba !pos len);
		with exn ->
			(* print a debugging message for why we failed *)
			Vt100.printf "ext2fs fail: pos = %d, block size = %d, len = %d, total len = %d\n"
				!pos block_size len inode.i_size;
			raise exn;
		end;
		(* update bookkeeping *)
		pos := !pos + len;
	in
	(* calculate the block ranges (in bytes) *)
	let direct_range = block_size * 12 in
	let indirect_range = block_size * block_size / 4 in
	let doubly_indirect_range = block_size * block_size * block_size / 16 in
	let triply_indirect_range = block_size * block_size * block_size * block_size / 64 in
	(* and map a block into an index *)
	let block_to_block_indices = function
		| n when n < 12 ->
			n, Direct n
		| n when (n - 12) < (block_size / 4) ->
			n, Indirect (n - 12)
		| n when (n - 12 - block_size / 4) < (block_size * block_size / 16) ->
			let n1, n2 = divmod (n - 12 - block_size / 4) (block_size / 4) in
			n, DoubleIndirect (n1, n2)
		| n when (n - 12) < (block_size / 64 * block_size) ->
			let n1, r = divmod (n - 12) (block_size / 4) in
			let n2, n3 = divmod r (block_size / 4) in
			n, TripleIndirect (n1, n2, n3)
		| n ->
			Vt100.printf "byte_to_block_indices: n = %d (%d, %d, %d, %d)\n" n
				direct_range indirect_range doubly_indirect_range triply_indirect_range;
			failwith "ext2fs: file too big"
	in
	(* find the start and end block indices *)
	let start = block_to_block_indices (ofs / block_size) in
	let finish = block_to_block_indices ((ofs + len + block_size) / block_size) in
	(* now we need a reading loop (and probably a cache) *)
	let rec read_loop = function
		| _, blk when blk = snd finish -> (* we're done I guess *) ()
		| x, Direct n ->
			if inode.i_block.(n) <> 0 then begin
				write inode.i_block.(n);
				read_loop (block_to_block_indices (x + 1))
			end
		| x, Indirect n ->
			(* now what...? :P *)
			let s = read inode.i_block.(12) in (* the indirect block *)
			let inp = IO.input_string (String.sub s (n * 4) 4) in (* only the block we want *)
			let block = IO.read_i32 inp in
			if block <> 0 then begin
				write block;
				read_loop (block_to_block_indices (x + 1))
			end
		| x, DoubleIndirect (n, o) ->
			let s1 = read inode.i_block.(13) in (* the first indirect block *)
			let inp = IO.input_string (String.sub s1 (n * 4) 4) in
			let s2 = read (IO.read_i32 inp) in
			let inp = IO.input_string (String.sub s2 (o * 4) 4) in
			let block = IO.read_i32 inp in
			if block <> 0 then begin
				write block;
				read_loop (block_to_block_indices (x + 1))
			end
		| x, TripleIndirect (n, o, p) ->
			let s1 = read inode.i_block.(14) in (* first indirect block *)
			let inp = IO.input_string (String.sub s1 (n * 4) 4) in
			let s2 = read (IO.read_i32 inp) in (* second indirect block *)
			let inp = IO.input_string (String.sub s2 (o * 4) 4) in
			let s3 = read (IO.read_i32 inp) in (* third indirect block *)
			let inp = IO.input_string (String.sub s3 (p * 4) 4) in
			let block = IO.read_i32 inp in
			if block <> 0 then begin
				write block;
				read_loop (block_to_block_indices (x + 1))
			end
		| _ -> failwith "ext2fs: reading loop failed"
	in
	(* do the reading... *)
	read_loop start;
	(* return the trimmed bigarray result *)
	Bigarray.Array1.sub ba (ofs mod block_size) len

let readfile_ba fs inode =
	let indirect_entries = (1024 lsl fs.s.s_log_block_size) / 4 in
	let num_sectors = 2 lsl fs.s.s_log_block_size in
	let block_size = 1024 lsl fs.s.s_log_block_size in
	let read ofs = fs.p.read (to_sector fs.s ofs) num_sectors in
	let ba = Bigarray.Array1.create Bigarray.int8_unsigned Bigarray.c_layout inode.i_size in
	let pos = ref 0 in
	let write ofs =
		let s = read ofs in
		let len = if inode.i_size - !pos < block_size
			then inode.i_size - !pos
			else block_size
		in
		begin try
			Bigarray.Array1.blit_from_string
				(if len = block_size then s else String.sub s 0 len)
				(Bigarray.Array1.sub ba !pos len);
		with exn ->
			Vt100.printf "ext2fs fail: pos = %d, block size = %d, len = %d, total len = %d\n"
				!pos block_size len inode.i_size;
			raise exn;
		end;
		pos := !pos + len;
	in
	(* read indirect block data into the bigarray *)
	let rec read_indirect indirect_block levels =
		let inp = IO.input_string (read indirect_block) in
		if levels = 0 then begin
			(* the int32 values point to blocks of data *)
			loop_until indirect_entries (fun _ ->
				let block = IO.read_i32 inp in
				if block <> 0 then begin
					write block; true
				end else false)
		end else begin
			loop_until indirect_entries (fun _ ->
				let indirect_block = IO.read_i32 inp in
				if indirect_block <> 0 then begin
					read_indirect indirect_block (levels - 1); true
				end else false)
		end
	in
	loop_until 12 (fun i ->
		if inode.i_block.(i) <> 0 then begin
			write inode.i_block.(i); true
		end else false);
	if inode.i_block.(12) <> 0 then read_indirect inode.i_block.(12) 0;
	if inode.i_block.(13) <> 0 then read_indirect inode.i_block.(13) 1;
	if inode.i_block.(14) <> 0 then read_indirect inode.i_block.(14) 2;
	(* return the bigarray result *)
	ba

(* override readfile_ba with a version that reads the file in chunks *)

let readfile_ba fs inode =
	let chunksize = 4096 in
	let ba = Bigarray.Array1.create Bigarray.int8_unsigned Bigarray.c_layout inode.i_size in
	let rec loop pos =
		if pos = inode.i_size then ()
		else begin
			let subba = read_file_range_ba fs inode pos chunksize in
			let len = Bigarray.Array1.dim subba in
			Bigarray.Array1.blit
				subba
				(Bigarray.Array1.sub ba pos len);
			loop (pos + len)
		end
	in loop 0;
	(* return the bigarray result *)
	ba

module KB = KernelBuffer

let read_file fs inode src buffer ofs len =
	(* pos_ofs + pos is the block to start at *)
	let pos_ofs = (ofs + src.KB.offset) / src.KB.units in
	(* num_blocks is how many blocks we need to read *)
	let num_blocks = len / src.KB.units + 1 in
	(* then the tricky part: get list of blocks to read, prolly sort them *)
	(* then using the sort, can figure out which sectors to read, could prolly be optimised *)
	
	0 (* don't do any work *)

let open_file fs inode =
	let rec src = {
		KB.position = 0;
		KB.length = inode.i_size; (* this is in bytes, not blocks *)
		KB.units = fs.bs;
		KB.offset = 0;
		KB.fill = begin fun buffer ofs len ->
				read_file fs inode src buffer ofs len
			end;
	} in src

let make p =
	let s = superblock p in
	let t = block_group_descriptor_table p s in
	let bs = 1024 lsl s.s_log_block_size in
	let i = inode { p = p; s = s; t = t; r = null_inode; bs = bs } 2l in
	{
		p = p;
		s = s;
		t = t;
		r = i;
		bs = bs (* sick of perpetually calculating this *)
	}

let create p =
	let m = make p in
	{
		metadata = m;
		read_dir = readdir m;
		read_inode = inode m;
		read_file = readfile m;
		read_file_ba = readfile_ba m;
	}
		
let init p =
	(* read the superblock, it's 2 sectors worth *)
	begin
		try
	let s = superblock p in
	(* print something kinda like mke2fs *)
	Vt100.printf "Block size=%d (log=%d)\n" (1024 lsl s.s_log_block_size) s.s_log_block_size;
	Vt100.printf "Fragment size=%d (log=%d)\n" (1024 lsl s.s_log_frag_size) s.s_log_frag_size;
	Vt100.printf "%d inodes, %d blocks\n" s.s_inodes_count s.s_blocks_count;
	Vt100.printf "%d blocks (%d%%) reserved for the super user\n" s.s_r_blocks_count ((s.s_r_blocks_count+1) * 100 / s.s_blocks_count);
	Vt100.printf "First data block=%d\n" s.s_first_data_block;
	Vt100.printf "Maximum filesystem blocks=? dunno how to calculate...\n"; (* dunno where this comes from *)
	Vt100.printf "%d block groups\n" (s.s_inodes_count / s.s_inodes_per_group);
	Vt100.printf "%d blocks per group, %d fragments per group\n" s.s_blocks_per_group s.s_frags_per_group;
	Vt100.printf "%d inodes per group\n" s.s_inodes_per_group;
	Vt100.printf "Superblock backups stored on blocks: ....\n";
	Vt100.printf "\n";
	Vt100.printf "This filesystem will be automatically checked every %d mounts or %d days,\n"
		s.s_max_mnt_count (s.s_checkinterval / 86400);
	Vt100.printf "whichever comes first.\n";
	let t = block_group_descriptor_table p s in
	(* print first four block group descriptors.... *)
	for i = 0 to 3 do
		Vt100.printf "block group descriptor %d:\n" i;
		let d = t.(i) in
		Vt100.printf "block bitmap@%x; inode bitmap@%x; inode table@%x\n"
			d.bg_block_bitmap d.bg_inode_bitmap d.bg_inode_table;
		Vt100.printf "free blocks: %d; free inodes: %d; used dirs: %d\n"
			d.bg_free_blocks_count d.bg_free_inodes_count d.bg_used_dirs_count;
	done;
	Vt100.printf "block group descriptor %d (second last):\n" (Array.length t - 2);
	let d = t.(Array.length t - 2) in
	Vt100.printf "block bitmap@%x; inode bitmap@%x; inode table@%x\n"
		d.bg_block_bitmap d.bg_inode_bitmap d.bg_inode_table;
	Vt100.printf "free blocks: %d; free inodes: %d; used dirs: %d\n"
		d.bg_free_blocks_count d.bg_free_inodes_count d.bg_used_dirs_count;
	Vt100.printf "block group descriptor %d (last):\n" (Array.length t - 1);
	let d = t.(Array.length t - 1) in
	Vt100.printf "block bitmap@%x; inode bitmap@%x; inode table@%x\n"
		d.bg_block_bitmap d.bg_inode_bitmap d.bg_inode_table;
	Vt100.printf "free blocks: %d; free inodes: %d; used dirs: %d\n"
		d.bg_free_blocks_count d.bg_free_inodes_count d.bg_used_dirs_count;
	let fs = { p = p; s = s; t = t; r = null_inode; bs = 1024 lsl s.s_log_block_size } in
	let root_dir_inode = inode fs 2l in
	let fs = { fs with r = root_dir_inode } in
	Vt100.printf "root dir inode: %s, size = %d, flags = %lx, uid = %d, gid = %d\n"
		(of_imode root_dir_inode.i_mode)
		root_dir_inode.i_size root_dir_inode.i_flags
		root_dir_inode.i_uid root_dir_inode.i_gid;
	let root_dir_list = readdir fs root_dir_inode in
	Vt100.printf "Directory listing for /:\n";
	List.iter begin fun entry ->
		Vt100.printf "%s\n" entry.name
	end root_dir_list;
		with Failure "not linux native partition" -> ()
		| ex -> Vt100.printf "error: %s" (Printexc.to_string ex)
	end
