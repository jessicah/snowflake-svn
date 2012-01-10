(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)

(* $Id: pervasives.ml,v 1.81 2006-11-17 08:34:01 weis Exp $ *)

(* type 'a option = None | Some of 'a *)

(* Exceptions *)

external raise : exn -> 'a = "%raise"

let failwith s = raise(Failure s)
let invalid_arg s = raise(Invalid_argument s)

exception Exit

(* Comparisons *)

external (=) : 'a -> 'a -> bool = "%equal"
external (<>) : 'a -> 'a -> bool = "%notequal"
external (<) : 'a -> 'a -> bool = "%lessthan"
external (>) : 'a -> 'a -> bool = "%greaterthan"
external (<=) : 'a -> 'a -> bool = "%lessequal"
external (>=) : 'a -> 'a -> bool = "%greaterequal"
external compare: 'a -> 'a -> int = "%compare"

let min x y = if x <= y then x else y
let max x y = if x >= y then x else y

external (==) : 'a -> 'a -> bool = "%eq"
external (!=) : 'a -> 'a -> bool = "%noteq"

(* Boolean operations *)

external not : bool -> bool = "%boolnot"
external (&) : bool -> bool -> bool = "%sequand"
external (&&) : bool -> bool -> bool = "%sequand"
external (or) : bool -> bool -> bool = "%sequor"
external (||) : bool -> bool -> bool = "%sequor"

(* Integer operations *)

external (~-) : int -> int = "%negint"
external succ : int -> int = "%succint"
external pred : int -> int = "%predint"
external (+) : int -> int -> int = "%addint"
external (-) : int -> int -> int = "%subint"
external ( * ) : int -> int -> int = "%mulint"
external (/) : int -> int -> int = "%divint"
external (mod) : int -> int -> int = "%modint"

let abs x = if x >= 0 then x else -x

external (land) : int -> int -> int = "%andint"
external (lor) : int -> int -> int = "%orint"
external (lxor) : int -> int -> int = "%xorint"

let lnot x = x lxor (-1)

external (lsl) : int -> int -> int = "%lslint"
external (lsr) : int -> int -> int = "%lsrint"
external (asr) : int -> int -> int = "%asrint"

let min_int = 1 lsl (if 1 lsl 31 = 0 then 30 else 62)
let max_int = min_int - 1

(* Floating-point operations *)

external (~-.) : float -> float = "%negfloat"
external (+.) : float -> float -> float = "%addfloat"
external (-.) : float -> float -> float = "%subfloat"
external ( *. ) : float -> float -> float = "%mulfloat"
external (/.) : float -> float -> float = "%divfloat"
external ( ** ) : float -> float -> float = "caml_power_float" "pow" "float"
external exp : float -> float = "caml_exp_float" "exp" "float"
external acos : float -> float = "caml_acos_float" "acos" "float"
external asin : float -> float = "caml_asin_float" "asin" "float"
external atan : float -> float = "caml_atan_float" "atan" "float"
external atan2 : float -> float -> float = "caml_atan2_float" "atan2" "float"
external cos : float -> float = "caml_cos_float" "cos" "float"
external cosh : float -> float = "caml_cosh_float" "cosh" "float"
external log : float -> float = "caml_log_float" "log" "float"
external log10 : float -> float = "caml_log10_float" "log10" "float"
external sin : float -> float = "caml_sin_float" "sin" "float"
external sinh : float -> float = "caml_sinh_float" "sinh" "float"
external sqrt : float -> float = "caml_sqrt_float" "sqrt" "float"
external tan : float -> float = "caml_tan_float" "tan" "float"
external tanh : float -> float = "caml_tanh_float" "tanh" "float"
external ceil : float -> float = "caml_ceil_float" "ceil" "float"
external floor : float -> float = "caml_floor_float" "floor" "float"
external abs_float : float -> float = "%absfloat"
external mod_float : float -> float -> float = "caml_fmod_float" "fmod" "float"
external frexp : float -> float * int = "caml_frexp_float"
external ldexp : float -> int -> float = "caml_ldexp_float"
external modf : float -> float * float = "caml_modf_float"
external float : int -> float = "%floatofint"
external float_of_int : int -> float = "%floatofint"
external truncate : float -> int = "%intoffloat"
external int_of_float : float -> int = "%intoffloat"
external float_of_bits : int64 -> float = "caml_int64_float_of_bits"
let infinity =
  float_of_bits 0x7F_F0_00_00_00_00_00_00L
let neg_infinity =
  float_of_bits 0xFF_F0_00_00_00_00_00_00L
let nan =
  float_of_bits 0x7F_F0_00_00_00_00_00_01L
let max_float =
  float_of_bits 0x7F_EF_FF_FF_FF_FF_FF_FFL
let min_float =
  float_of_bits 0x00_10_00_00_00_00_00_00L
let epsilon_float =
  float_of_bits 0x3C_B0_00_00_00_00_00_00L

type fpclass =
    FP_normal
  | FP_subnormal
  | FP_zero
  | FP_infinite
  | FP_nan
external classify_float: float -> fpclass = "caml_classify_float"

(* String operations -- more in module String *)

external string_length : string -> int = "%string_length"
external string_create: int -> string = "caml_create_string"
external string_blit : string -> int -> string -> int -> int -> unit
                     = "caml_blit_string" "noalloc"

let (^) s1 s2 =
  let l1 = string_length s1 and l2 = string_length s2 in
  let s = string_create (l1 + l2) in
  string_blit s1 0 s 0 l1;
  string_blit s2 0 s l1 l2;
  s

(* Character operations -- more in module Char *)

external int_of_char : char -> int = "%identity"
external unsafe_char_of_int : int -> char = "%identity"
let char_of_int n =
  if n < 0 || n > 255 then invalid_arg "char_of_int" else unsafe_char_of_int n

(* Unit operations *)

external ignore : 'a -> unit = "%ignore"

(* Pair operations *)

external fst : 'a * 'b -> 'a = "%field0"
external snd : 'a * 'b -> 'b = "%field1"

(* String conversion functions *)

external format_int: string -> int -> string = "caml_format_int"
external format_float: string -> float -> string = "caml_format_float"

let string_of_bool b =
  if b then "true" else "false"
let bool_of_string = function
  | "true" -> true
  | "false" -> false
  | _ -> invalid_arg "bool_of_string"

let string_of_int n =
  format_int "%d" n

external int_of_string : string -> int = "caml_int_of_string"

module String = struct
  external get : string -> int -> char = "%string_safe_get"
end

let valid_float_lexem s =
  let l = string_length s in
  let rec loop i =
    if i >= l then s ^ "." else
    match s.[i] with
    | '0' .. '9' | '-' -> loop (i+1)
    | _ -> s
  in
  loop 0
;;

let string_of_float f = valid_float_lexem (format_float "%.8g" f);;

external float_of_string : string -> float = "caml_float_of_string"

(* List operations -- more in module List *)

let rec (@) l1 l2 =
  match l1 with
    [] -> l2
  | hd :: tl -> hd :: (tl @ l2)

(* I/O operations *)

open Vfs

type in_channel = Vfs.io_channel
type out_channel = Vfs.io_channel

external magic : 'a -> 'b = "%identity"

let stdin  = { inode = Vfs.null_inode }
let stdout = { inode = Vfs.null_inode }
let stderr = { inode = Vfs.null_inode }

(* General output functions *)

type open_flag =
    Open_rdonly | Open_wronly | Open_append
  | Open_creat | Open_trunc | Open_excl
  | Open_binary | Open_text | Open_nonblock

let open_out_gen mode perm name =
	(* this is where we walk to get an inode, and try open it *)
	raise Not_supported

let open_out name =
  open_out_gen [Open_wronly; Open_creat; Open_trunc; Open_text] 0o666 name

let open_out_bin name =
  open_out_gen [Open_wronly; Open_creat; Open_trunc; Open_binary] 0o666 name

let flush oc =
	oc.inode.flush_out ()

(*external out_channels_list : unit -> out_channel list = "caml_ml_out_channels_list"*)
(*let flush_all () =
	let rec iter = function
		| [] -> ()
		| a::l -> (try flush a with _ -> ()); iter l
	in iter (out_channels_list ())*)
let flush_all () = ()

(*external unsafe_output : out_channel -> string -> int -> int -> unit*)
let unsafe_output oc s ofs len =
	let len = ref len in
	let pos = ref ofs in 
	while !len > 0 do
		let written = oc.inode.output_bytes s !pos !len in
		len := !len - written;
		pos := !pos + written;
	done

let output_char oc ch =
	oc.inode.output_byte (int_of_char ch)

let output_string oc s =
	unsafe_output oc s 0 (string_length s)

let output oc s ofs len =
	if ofs < 0 || len < 0 || ofs > string_length s - len
	then invalid_arg "output"
	else unsafe_output oc s ofs len

let output_byte oc byte =
	oc.inode.output_byte byte

let output_binary_int oc bin =
	output_byte oc ((bin asr 24) land 0xff);
	output_byte oc ((bin asr 16) land 0xff);
	output_byte oc ((bin asr 8) land 0xff);
	output_byte oc (bin land 0xff)

let output_value chan v = failwith "output_value not implemented"

let seek_out oc pos =
	oc.inode.seek_out pos
let pos_out oc =
	oc.inode.pos_out ()
let out_channel_length oc =
	oc.inode.length_out ()
let close_out_channel oc =
	oc.inode.close_out ()

let close_out oc = flush oc; close_out_channel oc
let close_out_noerr oc =
  (try flush oc with _ -> ());
  (try close_out_channel oc with _ -> ())
let set_binary_mode_out _ _ = ()

(* General input functions *)

let open_in_gen mode perm name =
	(* ignore mode & perm *)
	let path = Vfs.split_on_slash name in
	let inode = Vfs.walk path in
	if inode.is_directory () then
		raise Not_found
	else begin
		inode.open_in ();
		{ inode }
	end

let open_in name =
  open_in_gen [Open_rdonly; Open_text] 0 name

let open_in_bin name =
  open_in_gen [Open_rdonly; Open_binary] 0 name

let input_char ic =
	char_of_int (ic.inode.input_byte ())

let input ic s ofs len = invalid_arg "input"

let rec unsafe_really_input ic s ofs len =
	if len <= 0 then () else begin
		let r = ic.inode.input_bytes s ofs len in
		if r = 0 then raise End_of_file
		else unsafe_really_input ic s (ofs+r) (len-r)
	end

let really_input ic s ofs len =
	if ofs < 0 || len < 0 || ofs > string_length s - len
	then invalid_arg "really_input"
	else unsafe_really_input ic s ofs len

let input_scan_line _ = 0

let input_line _ = raise End_of_file

let input_byte ic = ic.inode.input_byte ()
let input_binary_int _ = raise End_of_file
let input_value _ = raise End_of_file
let seek_in ic offset = ic.inode.seek_in offset
let pos_in ic = ic.inode.pos_in ()
let in_channel_length ic = ic.inode.length_in ()
let close_in _ = ()
let close_in_noerr _ = ()
let set_binary_mode_in _ _ = ()

(* Output functions on standard output *)

let print_char c = output_char stdout c
let print_string s = output_string stdout s
let print_int i = output_string stdout (string_of_int i)
let print_float f = output_string stdout (string_of_float f)
let print_endline s =
  output_string stdout s; output_char stdout '\n'; flush stdout
let print_newline () = output_char stdout '\n'; flush stdout

(* Output functions on standard error *)

let prerr_char c = output_char stderr c
let prerr_string s = output_string stderr s
let prerr_int i = output_string stderr (string_of_int i)
let prerr_float f = output_string stderr (string_of_float f)
let prerr_endline s =
  output_string stderr s; output_char stderr '\n'; flush stderr
let prerr_newline () = output_char stderr '\n'; flush stderr

(* Input functions on standard input *)

let read_line () = flush stdout; input_line stdin
let read_int () = int_of_string(read_line())
let read_float () = float_of_string(read_line())

(* Operations on large files *)

module LargeFile =
  struct
		let seek_out _ _ = ()
		let pos_out _ = 0L
		let out_channel_length _ = 0L
		
		let seek_in _ _ = ()
		let pos_in _ = 0L
		let in_channel_length _ = 0L
  end

(* References *)

type 'a ref = { mutable contents: 'a }
external ref: 'a -> 'a ref = "%makemutable"
external (!): 'a ref -> 'a = "%field0"
external (:=): 'a ref -> 'a -> unit = "%setfield0"
external incr: int ref -> unit = "%incr"
external decr: int ref -> unit = "%decr"

(* Formats *)
type ('a, 'b, 'c, 'd) format4 = ('a, 'b, 'c, 'c, 'c, 'd) format6 

type ('a, 'b, 'c) format = ('a, 'b, 'c, 'c) format4

external format_of_string :
 ('a, 'b, 'c, 'd, 'e, 'f) format6 ->
 ('a, 'b, 'c, 'd, 'e, 'f) format6 = "%identity"

external format_to_string :
 ('a, 'b, 'c, 'd, 'e, 'f) format6 -> string = "%identity"
external string_to_format :
 string -> ('a, 'b, 'c, 'd, 'e, 'f) format6 = "%identity"

let (( ^^ ) :
      ('a, 'b, 'c, 'd, 'e, 'f) format6 ->
      ('f, 'b, 'c, 'e, 'g, 'h) format6 ->
      ('a, 'b, 'c, 'd, 'g, 'h) format6) =
  fun fmt1 fmt2 ->
    string_to_format (format_to_string fmt1 ^ format_to_string fmt2);;

let string_of_format fmt =
  let s = format_to_string fmt in
  let l = string_length s in
  let r = string_create l in
  string_blit s 0 r 0 l;
  r

(* Miscellaneous *)

(*external sys_exit : int -> 'a = "caml_sys_exit"*)

let exit_function = ref ignore

let at_exit f =
  let g = !exit_function in
  exit_function := (fun () -> f(); g())

let do_at_exit () = (!exit_function) ()

let exit retcode =
  do_at_exit ();
  failwith "can't exit the kernel"

external register_named_value : string -> 'a -> unit
                              = "caml_register_named_value"

let _ = register_named_value "Pervasives.do_at_exit" do_at_exit
