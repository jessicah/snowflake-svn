
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
	(*font = BDF.get "Courier" BDF.Normal BDF.Regular 14;*)
	font = BDF.get "Unifont" BDF.Normal BDF.Regular 16;
	colour = 255, 255, 255;
}

let init () =
	t.frame_buffer <- Asm.matrix32 (set_mode 0x144) 768 1024

let gfx_console =
	object (self)
		inherit Ovt100.console as super
		val mutable width = 1024 / t.font.BDF.global_bbox.BDF.width * 2
		val mutable height = 768 / t.font.BDF.global_bbox.BDF.height
		method attrib x = () (* don't care about setting colours right now *)
		method erase length = () (* FIXME! *)
		method draw uchar =
			let p = draw_uchar
				t.frame_buffer
				(UChar.int_of_uchar uchar)
				t.font
				(super#get_x * t.font.BDF.global_bbox.BDF.width / 2, super#get_y * t.font.BDF.global_bbox.BDF.height + t.font.BDF.ascent)
				t.colour
			in
			(* the global bounding box should be for a "normal" character IMO, grrr *)
			(fst p) / (t.font.BDF.global_bbox.BDF.width / 2)
		method update_cursor =
			(* this implements scrolling and stuff.... UGH *)
			if super#get_x >= width then begin
				super#set_x (super#get_x mod width);
				super#move_y 1;
			end;
			if super#get_y >= height then begin
				super#move_y (-1);
				(* scroll... FIXME *)
				(* the text based graphics code:
				Array2.blit
					(Array2.sub_left console.term 1 24)
					(Array2.sub_left console.term 0 24);
				let last_row = Array2.slice_left console.term 24 in
				for i = 0 to Array1.dim last_row / 2 - 1 do
					last_row.{i * 2} <- ' ';
					last_row.{i * 2 + 1} <- char_of_int console.attrib;
				done*)
				Array2.blit
					(Array2.sub_left t.frame_buffer t.font.BDF.global_bbox.BDF.height (t.height - t.font.BDF.global_bbox.BDF.height))
					(Array2.sub_left t.frame_buffer 0 (t.height - t.font.BDF.global_bbox.BDF.height));
				Array2.fill
					(Array2.sub_left t.frame_buffer (super#get_y * t.font.BDF.global_bbox.BDF.height) (t.font.BDF.global_bbox.BDF.height))
					0x000000l;
			end
	end

(*let gfx_console =
	object (self)
		inherit Console.console as super
		method draw uchar =
			if (UChar.int_of_uchar uchar) = Char.code '\n'
				then t.position <- 50,24 + snd t.position
			else
				t.position <- draw_uchar
					t.frame_buffer
					(UChar.int_of_uchar uchar)
					t.font
					t.position
					t.colour;
			match t.position with
				| x,y when x >= t.width - 50 ->
						t.position <- 50, y + 24
				| _ -> ()
	end*)

(*let put ch =
	if ch = '\n' then t.position <- 50, 24 + snd t.position else
	t.position <- draw_char
		t.frame_buffer
		ch
		t.font
		t.position
		t.colour;
	match t.position with
		| x, y when x >= t.width - 50 ->
				t.position <- 50, y + 24
		| _ -> ()

let uput uchar =
	if (UChar.int_of_uchar uchar) = Char.code '\n' then t.position <- 50, 24 + snd t.position else
	t.position <- draw_uchar
		t.frame_buffer
		(UChar.int_of_uchar uchar)
		t.font
		t.position
		t.colour;
	match t.position with
		| x, y when x >= t.width - 50 ->
				t.position <- 50, y + 24
		| _ -> ()*)

let put ch = Ovt100.process gfx_console (UChar.uchar_of_int (Char.code ch))
let uput uchar = Ovt100.process gfx_console uchar

let out = IO.from_out_chars (object
		method put ch = put ch
		method flush () = ()
		method close_out () = ()
	end)

let printf fmt = IO.printf out fmt
