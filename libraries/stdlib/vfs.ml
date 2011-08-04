
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

type abstract_inode

exception Not_supported

module type Inode = sig
	type t
	
	(* Input *)
	val open_in : t -> unit
	val close_in : t -> unit
	val input_byte : t -> int
	val input_bytes : t -> string -> int -> int -> int
	val seek_in : t -> int -> unit
	val pos_in : t -> int
	val length_in : t -> int
	
	(* Output *)
	val open_out : t -> unit
	val close_out : t -> unit
	val flush_out : t -> unit
	val output_byte : t -> int -> unit
	val output_bytes : t -> string -> int -> int -> int
	val seek_out : t -> int -> unit
	val pos_out : t -> int
	val length_out : t -> int
	
	(* Coerce between abstract_inode & t *)
	val of_abstract_inode : abstract_inode -> t
	val to_abstract_inode : t -> abstract_inode
end

(*module NullInode : Inode = struct
	type t = abstract_inode
	
	let open_in _ = raise Not_supported
	let close_in _ = raise Not_supported
	let input_byte _ = raise Not_supported
	let input_bytes _ _ _ _ = raise Not_supported
	let seek_in _ _ = raise Not_supported
	let pos_in _ = raise Not_supported
	let length_in _ = raise Not_supported
	
	let open_out _ = raise Not_supported
	let close_out _ = raise Not_supported
	let flush_out _ = raise Not_supported
	let output_byte _ = raise Not_supported
	let output_bytes _ _ _ _ = raise Not_supported
	let seek_out _ _ = raise Not_supported
	let pos_out _ = raise Not_supported
	let length_out _ = raise Not_supported
	
	let of_abstract_inode x = x
	let to_abstract_inode x = x
end*)

module type FileSystem = sig
	type inode
	
	module Ops : Inode with type t = inode (* should it use = or := ... *)
	
	val walk : string list -> abstract_inode option (* raising Not_found is probably better than the option type? *)
	
	val is_directory : abstract_inode -> bool
	
	val read_dir : abstract_inode -> string list
		
end

type io_channel = {
	ops : (module Inode);
	inode : abstract_inode;
}

(* remember, no Pervasives here... *)

type 'a ref = { mutable contents: 'a }
external ref: 'a -> 'a ref = "%makemutable"
external (!): 'a ref -> 'a = "%field0"
external (:=): 'a ref -> 'a -> unit = "%setfield0"
external raise : exn -> 'a = "%raise"
external (=) : 'a -> 'a -> bool = "%equal"

let filesystems = ref ([] : (string * (module FileSystem)) list)

let mount filesystem path =
	(* if String.contains '/' path then failwith "invalid mount path"; *)
	(* if List.assoc_mem path !filesystems then failwith "already mounted in path"; *)
	filesystems := (path, filesystem) :: !filesystems

let unmount path =
	let fs = !filesystems in
	let rec remove = function
		| [] -> ()
		| (p, _) :: rest when p = path -> remove rest
		| x :: rest -> filesystems := x :: !filesystems; remove rest
	in
	filesystems := [];
	remove fs

let rec assoc x = function
    [] -> raise Not_found
  | (a,b)::l -> if a = x then b else assoc x l

let walk path_list =
	let root :: paths = path_list in
	let module FS = (val (assoc root !filesystems) : FileSystem) in
	FS.walk paths, (module FS : FileSystem)

let read_dir path_list =
	match (walk path_list) with
	| Some inode, fs -> 
		let module FS = (val fs : FileSystem) in FS.read_dir inode
	| None, _ -> raise Not_found

(* we can't implement is_directory & read_dir in current state! *)
