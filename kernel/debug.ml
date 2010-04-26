
open IO
open Asm
open Char

let logging = ref false

let printf fmt = printf begin from_out_chars (object
		method put ch = if ch = '\n' then out8 0x3f8 (code '\r'); out8 0x3F8 (code ch)
		method flush () = ()
		method close_out () = ()
		end)
	end fmt

let log name start finish = if !logging then
	printf "%s\t%Ld\t%Ld\t%Ld\n" name start finish (Int64.sub finish start)
