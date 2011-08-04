
open IO
open Asm
open Char

let logging = ref false

let output_byte byte =
	if byte = code '\n' then out8 0x3f8 (code '\r'); out8 0x3f8 byte
	
let output_char ch = output_byte (code ch)

let printf fmt = printf begin from_out_chars (object
		method put ch = output_char ch
		method flush () = ()
		method close_out () = ()
		end)
	end fmt

let log name start finish = if !logging then
	printf "%s\t%Ld\t%Ld\t%Ld\n" name start finish (Int64.sub finish start)

(* redefined Pervasives.stderr at runtime :D *)
module StdErr = struct
	type t = unit
	
	open Vfs
	
	let open_in _ = raise Not_supported
	let close_in _ = raise Not_supported
	let input_byte _ = raise Not_supported
	let input_bytes _ _ _ _ = raise Not_supported
	let seek_in _ _ = raise Not_supported
	let pos_in _ = raise Not_supported
	let length_in _ = raise Not_supported
	
	let open_out _ = raise Not_supported
	let close_out _ = raise Not_supported
	let flush_out _ = ()
	let output_byte _ byte = output_byte byte		
	let output_bytes _ buf ofs len =
		String.iter output_char (String.sub buf ofs len); len
	let seek_out _ _ = raise Not_supported
	let pos_out _ = raise Not_supported
	let length_out _ = raise Not_supported
	
	let of_abstract_inode _ = ()
	let to_abstract_inode () = Obj.magic ()
end

let stderr' = { Vfs.ops = (module StdErr : Vfs.Inode); inode = Obj.magic () }

module type PERVASIVES = sig
	include module type of Pervasives
end

let pervasives = (module Pervasives : PERVASIVES)

let init () =
	(* runtime modification : stderr is field 24 in Pervasives *)
	let t = Obj.repr pervasives in
	Obj.set_field t 24 (Obj.repr stderr')
