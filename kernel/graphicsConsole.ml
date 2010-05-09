
(* a very basic graphical console *)

open Bigarray
open Fonts

external set_mode : int -> int32 = "snowflake_vbe_switch"

type t = {
	mutable position: int * int;
	mutable width: int;
	mutable height: int;
	mutable frame_buffer : (int32, int32_elt, c_layout) Array2.t;
	mutable font : BDF.font;
	mutable colour : int * int * int;
}

let t = {
	position = 50, 50;
	width = 1024;
	height = 768;
	frame_buffer = Array2.create int32 c_layout 1 1;
	font = BDF.get "Courier" BDF.Normal BDF.Regular 14;
	colour = 255, 255, 255;
}

let init () =
	t.frame_buffer <- Asm.matrix32 (set_mode 0x144) 768 1024

let put ch =
	t.position <- draw_char
		t.frame_buffer
		ch
		t.font
		t.position
		t.colour;
	if ch = '\n' then t.position <- 50, 24 + snd t.position;
	match t.position with
		| x, y when x >= t.width - 50 ->
				t.position <- 50, y + 24
		| _ -> ()

let out = IO.from_out_chars (object
		method put ch = put ch
		method flush () = ()
		method close_out () = ()
	end)

let printf fmt = IO.printf out fmt
