
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
	i_faddr : int;
	i_dir_acl : int;
	i_file_acl : int;
	i_generation : int;
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
	inode : int;
}

(* IO helper function *)

let read_bytes i n =
	let rec read acc = function
		| 0 -> List.rev acc
		| n -> read (IO.read_byte i :: acc) (n-1)
	in read [] n

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
	(* located on the first block following the superblock *)
	(* superblock at 1024 bytes from partition start *)
	let block_size = 1024 lsl s.s_log_block_size in
	let superblock_block = 1024 / block_size in
	(* if blocksize = 1024, then = 1, else 0 *)
	(* => bgd at 2 (third block), else 1 (second block) *)
	let table_block = superblock_block + 1 in
	(* block group descriptor is 32 bytes *)
	(* num block groups = s_inodes_count / s_inodes_per_group *)
	let num_groups = s.s_inodes_count / s.s_inodes_per_group in
	let length = num_groups * 32 in
	let num_sectors = ((length + 511) / 512) in
	let table_raw = String.create (512 * num_sectors) in
	let sector_start = ((table_block * block_size) / 512) in
	Vt100.printf "reading %d sectors, starting at offset %d\n"
		num_sectors sector_start;
	(* lifted this out in an attempt to work around IDE problems; no such luck! =/ *)
	for i = 0 to num_sectors - 1 do
		Vt100.printf "sector %d...\n" (sector_start + i);
		let sector = p.read (sector_start + i) 1 in
		String.blit
			sector 0
			table_raw (i * 512)
			512;
	done;
	
	(* assume descriptors are all one after another, no padding/gaps *)
	(*let table_raw = p.read ((table_block * block_size) / 512) ((length + 511) / 512) in*)
	let i = IO.input_string table_raw in
	(* Array.init works in the expected order *)
	Array.init num_groups begin fun group ->
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

let inode p s t x =
	let block_group = (x - 1) / s.s_inodes_per_group in
	let inode_index = (x - 1) mod s.s_inodes_per_group in
	let descr = t.(block_group) in
	let inode_size =
		if s.s_rev_level >= 1 (* ext2_dynamic_rev *)
		then s.s_inode_size
		else 128 in
	(* bg_inode_table is the first block of the inode table *)
	let pos = (descr.bg_inode_table * (1024 lsl s.s_log_block_size)) + (inode_index * inode_size) in
	Vt100.printf "inode %d located at offset %d, block group = %d, inode index = %d\n" x pos block_group inode_index;
	Vt100.printf "inode table = %x, block size = %d (log = %d), inode_size = %d\n" descr.bg_inode_table (1024 lsl s.s_log_block_size) s.s_log_block_size inode_size;
	(* an inode always fits within a sector, so it works okay *)
	let sector = p.read (pos / 512) 1 in
	let offset = pos mod 512 in
	let i = IO.input_string sector in
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
		i_block = Array.init 15 (fun _ -> read_i32 i);
		i_generation = read_i32 i;
		i_file_acl = read_i32 i;
		i_dir_acl = read_i32 i;
		i_faddr = read_i32 i;
		i_osd2 = read_bytes i 12;
	}

let readdir p s t inode =
	let buffer = String.make inode.i_size '\000' in
	let block_size = 1024 lsr s.s_log_block_size in
	(* copy the data into the buffer *)
	for i = 0 to (inode.i_size / block_size) - 1 do
		(* get the block from i_block *)
		if i < 13 then begin
			let block = inode.i_block.(i) in
			if block <> 0 then begin
				(* got a block number *)
				()
			end
			(* do nothing for 'sparse' blocks *)
		end else begin
			(* for 13 and above, we need to use indirection blocks... *)
			failwith "can't do indirect blocks yet"
		end
	done;
	()

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
	let root_dir_inode = inode p s t 2 in
	Vt100.printf "root dir inode: %s, size = %d, flags = %lx, uid = %d, gid = %d\n"
		(of_imode root_dir_inode.i_mode)
		root_dir_inode.i_size root_dir_inode.i_flags
		root_dir_inode.i_uid root_dir_inode.i_gid
		with Failure "not linux native partition" -> ()
		| ex -> Vt100.printf "error: %s" (Printexc.to_string ex)
	end
