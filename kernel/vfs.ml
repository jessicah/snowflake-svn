
(* Virtual File System Layer -- or smth kinda like it *)

(*
	API layers to implement:
	
	Module Sys
	----------
	
	val file_exists : string -> bool
	val is_directory : string -> bool
	val remove : string -> unit
	val rename : string -> string -> unit
	val chdir : string -> unit ; okay, mebbe not this one
	val getcwd : unit -> string ; mebbe not this one either
	val readdir : string -> string array
	
	Module Pervasives
	-----------------
	
	type in_channel
	type out_channel
	
	(* The following are specialised I/O routines *)
	val stdin : in_channel
	val stdout : out_channel
	val stderr : out_channel
	val print_char : char -> unit
	val print_string : string -> unit
	val print_int : int -> unit
	val print_float : float -> unit
	val print_endline : string -> unit
	val print_newline : unit -> unit
	val prerr_char : char -> unit
	val prerr_string : string -> unit
	val prerr_int : int -> unit
	val prerr_float : float -> unit
	val prerr_endline : string -> unit
	val prerr_newline : unit -> unit
	val read_line : unit -> string
	val read_int : unit -> int
	val read_float : unit -> float
	
	type open_flag =
			Open_rdonly
		| Open_wronly
		| Open_append
		| Open_creat
		| Open_trunc
		| Open_excl
		| Open_binary
		| Open_text
		| Open_nonblock
	val open_out : string -> out_channel
	val open_out_bin : string -> out_channel
	val open_out_gen : open_flag list -> int -> string -> out_channel
	val flush : out_channel -> unit
	val flush_all : unit -> unit
	val output_char : out_channel -> char -> unit
	val output_string : out_channel -> string -> unit
	val output : out_channel -> string -> int -> int -> unit
	val output_byte : out_channel -> int -> unit
	val output_binary_int : out_channel -> int -> unit
	val output_value : out_channel -> 'a -> unit
	val seek_out : out_channel -> int -> unit
	val pos_out : out_channel -> int
	val out_channel_length : out_channel -> int
	val close_out : out_channel -> unit
	val close_out_noerr : out_channel -> unit
	val set_binary_mode_out : out_channel -> bool -> unit
	val open_in : string -> in_channel
	val open_in_bin : string -> in_channel
  val open_in_gen : open_flag list -> int -> string -> in_channel
	val input_char : in_channel -> char
	val input_line : in_channel -> string
	val input : in_channel -> string -> int -> int -> int
	val really_input : in_channel -> string -> int -> int -> unit
	val input_byte : in_channel -> int
	val input_binary_int : in_channel -> int
	val input_value : in_channel -> 'a
	val seek_in : in_channel -> int -> unit
	val pos_in : in_channel -> int
	val in_channel_length : in_channel -> int
	val close_in : in_channel -> unit
	val close_in_noerr : in_channel -> unit
	val set_binary_mode_in : in_channel -> bool -> unit
	module LargeFile :
		sig
			val seek_out : out_channel -> int64 -> unit
			val pos_out : out_channel -> int64
			val out_channel_length : out_channel -> int64
			val seek_in : in_channel -> int64 -> unit
			val pos_in : in_channel -> int64
			val in_channel_length : in_channel -> int64
		end
	val unsafe_really_input : in_channel -> string -> int -> int -> unit
*)

(* these two types should be concrete here, and abstract in Pervasives *)
type in_channel
type out_channel

module type FileSystem = sig
	type filesystem	
	type inode
	
	val walk : filesystem -> string list -> inode option
	
	val to_in_channel : inode -> in_channel
	val to_out_channel : inode -> out_channel
	
	val is_directory : inode -> bool
	
	val to_path : inode -> string list
	val to_name : inode -> string
	
	val read_dir : inode -> inode list
end

(* remember, no Pervasives here... *)

type 'a ref = { mutable contents: 'a }
external ref: 'a -> 'a ref = "%makemutable"
external (!): 'a ref -> 'a = "%field0"
external (:=): 'a ref -> 'a -> unit = "%setfield0"

let filesystems = ref ([] : (string * module FileSystem) list)

let mount filesystem path = ()
	(*
		1. check path doesn't contain path separators
		2. check path isn't used
		3. add to [filesystems] value above
	*)

let unmount path = ()

let walk path_list = ()
	(*
		1. get root path
		2. find it in [filesystems]
		3. invoke filesystem's walk function
	*)
