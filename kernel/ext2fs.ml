
(* An ext2 file system driver... need somewhere to start with this whole file system
	stuff, the eventual virtual file system design, and all that cruft... *)

open Partitions
open IO

let read_bytes i n =
	let rec read acc = function
		| 0 -> List.rev acc
		| n -> read (IO.read_byte i :: acc) (n-1)
	in read [] n

let init p =
	(* read the superblock, it's 2 sectors worth *)
	begin
		try
	if p.info.code <> 0x83 then failwith "not linux native partition";
	let superblock_raw = p.read 2 2 in
	let i = IO.input_string superblock_raw in
	let s_inodes_count = read_i32 i in
	let s_blocks_count = read_i32 i in
	let s_r_blocks_count = read_i32 i in
	let s_free_blocks_count = read_i32 i in
	let s_free_inodes_count = read_i32 i in
	let s_first_data_block = read_i32 i in
	let s_log_block_size = read_i32 i in
	let s_log_frag_size = read_i32 i in
	let s_blocks_per_group = read_i32 i in
	let s_frags_per_group = read_i32 i in
	let s_inodes_per_group = read_i32 i in
	let s_mtime = read_real_i32 i in
	let s_wtime = read_real_i32 i in
	let s_mnt_count = read_i16 i in
	let s_max_mnt_count = read_i16 i in
	let s_magic = read_i16 i in
	let s_state = read_i16 i in
	let s_errors = read_i16 i in
	let s_minor_rev_level = read_i16 i in
	let s_lastcheck = read_real_i32 i in
	let s_checkinterval = read_i32 i in
	let s_creator_os = read_real_i32 i in
	let s_rev_level = read_real_i32 i in
	let s_def_resuid = read_i16 i in
	let s_def_resgid = read_i16 i in
	(* ext2_dynamic_rev specific... *)
	let s_first_ino = read_i32 i in
	let s_inode_size = read_i16 i in
	let s_block_group_nr = read_i16 i in
	let s_feature_compat = read_real_i32 i in
	let s_feature_incompat = read_real_i32 i in
	let s_feature_ro_compat = read_real_i32 i in
	let s_uuid = read_bytes i 16 in
	let s_volume_name = read_bytes i 16 in
	let s_last_mounted = read_bytes i 64 in
	let s_algo_bitmap = read_real_i32 i in
	(* performance hints *)
	let s_prealloc_blocks = read_byte i in
	let s_prealloc_dir_blocks = read_byte i in
	ignore (read_bytes i 2);
	(* journalling support *)
	let s_journal_uuid = read_bytes i 16 in
	let s_journal_inum = read_real_i32 i in
	let s_journal_dev = read_real_i32 i in
	let s_last_orphan = read_real_i32 i in
	(* directory indexing support *)
	let hash_seed = read_bytes i 16 in
	let s_def_hash_version = read_byte i in
	ignore (read_bytes i 3);
	(* other options *)
	let s_default_mount_options = read_real_i32 i in
	let s_first_meta_bg = read_real_i32 i in
	(* rest. 760 bytes, is unused *)
	(* print something kinda like mke2fs *)
	Vt100.printf "Block size=%d (log=%d)\n" (1024 lsl s_log_block_size) s_log_block_size;
	Vt100.printf "Fragment size=%d (log=%d)\n" (1024 lsl s_log_frag_size) s_log_frag_size;
	Vt100.printf "%d inodes, %d blocks\n" s_inodes_count s_blocks_count;
	Vt100.printf "%d blocks (%d%%) reserved for the super user\n" s_r_blocks_count ((s_r_blocks_count+1) * 100 / s_blocks_count);
	Vt100.printf "First data block=%d\n" s_first_data_block;
	Vt100.printf "Maximum filesystem blocks=? dunno how to calculate...\n"; (* dunno where this comes from *)
	Vt100.printf "%d block groups\n" (s_inodes_count / s_inodes_per_group); (* this is off by one :( *)
	Vt100.printf "%d blocks per group, %d fragments per group\n" s_blocks_per_group s_frags_per_group;
	Vt100.printf "%d inodes per group\n" s_inodes_per_group;
	Vt100.printf "Superblock backups stored on blocks: ....\n";
	Vt100.printf "\n";
	Vt100.printf "This filesystem will be automatically checked every %d mounts or %d days,\n"
		s_max_mnt_count (s_checkinterval / 86400);
	Vt100.printf "whichever comes first.\n";
		with Failure "not linux native partition" -> ()
		| ex -> Vt100.printf "error: %s" (Printexc.to_string ex)
	end
