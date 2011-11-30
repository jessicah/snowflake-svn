
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

(* a filesystem will walk to an inode and return a record of function handles;
	the filesystem can use currying to hide filesystem-specific info that it needs	
 *)

type inode = {
	open_in : unit -> unit;
	close_in : unit -> unit;
	input_byte : unit -> int;
	input_bytes : string -> int -> int -> int;
	seek_in : int -> unit;
	pos_in : unit -> int;
	length_in : unit -> int;

	open_out : unit -> unit;
	close_out : unit -> unit;
	flush_out : unit -> unit;
	output_byte : int -> unit;
	output_bytes : string -> int -> int -> int;
	seek_out : int -> unit;
	pos_out : unit -> int;
	length_out : unit -> int;

	is_directory : unit -> bool;

	read_dir : unit -> string list;
}

external raise : exn -> 'a = "%raise"

let null_inode = {
	open_in = (fun _ -> raise Not_supported);
	close_in = (fun _ -> raise Not_supported);
	input_byte = (fun _ -> raise Not_supported);
	input_bytes = (fun _ _ _ -> raise Not_supported);
	seek_in = (fun _ -> raise Not_supported);
	pos_in = (fun _ -> raise Not_supported);
	length_in = (fun _ -> raise Not_supported);
	open_out = (fun _ -> raise Not_supported);
	close_out = (fun _ -> raise Not_supported);
	flush_out = (fun _ -> raise Not_supported);
	output_byte = (fun _ -> raise Not_supported);
	output_bytes = (fun _ _ _ -> raise Not_supported);
	seek_out = (fun _ -> raise Not_supported);
	pos_out = (fun _ -> raise Not_supported);
	length_out = (fun _ -> raise Not_supported);
	is_directory = (fun _ -> raise Not_supported);
	read_dir = (fun _ -> raise Not_supported);
}

type file_system = {
	walk : string list -> inode;
}

type io_channel = {
	mutable inode : inode;
}

(* remember, no Pervasives here... *)

type 'a ref = { mutable contents: 'a }
external ref: 'a -> 'a ref = "%makemutable"
external (!): 'a ref -> 'a = "%field0"
external (:=): 'a ref -> 'a -> unit = "%setfield0"
external raise : exn -> 'a = "%raise"
external (=) : 'a -> 'a -> bool = "%equal"


let filesystems = ref ([] : (string * file_system) list)

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

let walk = function
	| [] -> raise Not_found
	| root :: paths ->
		let fs = assoc root !filesystems in
		fs.walk paths

let read_dir path_list =
	let inode = walk path_list in
	if inode.is_directory () then
		inode.read_dir ()
	else
		raise Not_found

(* we can't implement is_directory & read_dir in current state! *)

external (=) : 'a -> 'a -> bool = "%equal"
external (<>) : 'a -> 'a -> bool = "%notequal"
external (<) : 'a -> 'a -> bool = "%lessthan"
external (>) : 'a -> 'a -> bool = "%greaterthan"
external (<=) : 'a -> 'a -> bool = "%lessequal"
external (>=) : 'a -> 'a -> bool = "%greaterequal"

external (&&) : bool -> bool -> bool = "%sequand"
external (||) : bool -> bool -> bool = "%sequor"

external (+) : int -> int -> int = "%addint"
external (-) : int -> int -> int = "%subint"


let failwith s = raise(Failure s)
let invalid_arg s = raise(Invalid_argument s)

(* taken from string module *)

external length : string -> int = "%string_length"
external create : int -> string = "caml_create_string"
external unsafe_get : string -> int -> char = "%string_unsafe_get"
external unsafe_set : string -> int -> char -> unit = "%string_unsafe_set"
external unsafe_blit : string -> int -> string -> int -> int -> unit
                     = "caml_blit_string" "noalloc"
external unsafe_fill : string -> int -> int -> char -> unit
                     = "caml_fill_string" "noalloc"

let make n c =
  let s = create n in
  unsafe_fill s 0 n c;
  s

let copy s =
  let len = length s in
  let r = create len in
  unsafe_blit s 0 r 0 len;
  r

let sub s ofs len =
  if ofs < 0 || len < 0 || ofs > length s - len
  then invalid_arg "String.sub"
  else begin
    let r = create len in
    unsafe_blit s ofs r 0 len;
    r
  end
let rec index_rec s lim i c =
  if i >= lim then raise Not_found else
  if unsafe_get s i = c then i else index_rec s lim (i+1) c;;

let index s c = index_rec s (length s) 0 c;;

let index_from s i c =
  if i < 0 || i > length s then invalid_arg "String.index_from" else
  index_rec s (length s) i c;;

(* taken from tarfs code *)
let split_on_slash path =
	let len = length path in
	let rec split ix =
		try
			let slash = index_from path ix '/' in
			if slash = 0 then split (slash + 1)
			else sub path ix (slash - ix) :: split (slash + 1)
		with Not_found -> sub path ix (len - ix) :: []
	in
	if len = 0 then []
	else if len = 1 && (unsafe_get path 0) = '/' then []
	else split 0
