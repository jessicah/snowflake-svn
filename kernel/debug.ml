
open IO
open Asm
open Char

let printf fmt = printf begin from_out_chars (object
		method put ch = if ch = '\n' then out8 0x3f8 (code '\r'); out8 0x3F8 (code ch)
		method flush () = ()
		method close_out () = ()
		end)
	end fmt
