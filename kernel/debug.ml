
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

open Vfs

let stderr = {
	null_inode with
	flush_out = ignore;
	output_byte;
	output_bytes = (fun buf ofs len -> String.iter output_char (String.sub buf ofs len); len);
}

let init () =
	(* runtime modification : stderr is field 24 in Pervasives *)
	let p_stderr : Vfs.io_channel = Obj.magic Pervasives.stderr in
	p_stderr.inode <- stderr
