
type colour = Black | Red | Green | Yellow | Blue | Magenta | Cyan | White

(*
	foreground 30-37; background 40-47
	bright send ESC[1;<n>m
					black		red		green		yellow	blue	magenta		cyan	white
	colour   30      31     32      33     34     35       36     37
*)
	
let set_attrib console attr =
	let ansi_to_colour = [| Black; Red; Green; Yellow; Blue; Magenta; Cyan; White |] in
	match attr with
	| 0 -> Some White, Some Black
	| 1 -> None, None (* a brighter colour *)
	| 7 -> Some console#get_background, Some console#get_colour
	| n when n >= 30 && n <= 37 ->
		Some ansi_to_colour.(n-30), None
	| n when n >= 40 && n <= 47 ->
		None, Some ansi_to_colour.(n-40)
	| _ -> None, None
	
class virtual console = object(self)
	val mutable pen = 0, 0
	val mutable saved = 0, 0
	val mutable colour = White
	val mutable background = Black
	val mutable virtual width : int
	val mutable virtual height : int
	method virtual set_colour : colour -> unit
	method virtual set_background : colour -> unit
	method virtual erase : int -> unit
	method virtual draw : UChar.uchar -> int
	method virtual update_cursor : unit
	method get_colour = colour
	method get_background = background
	method attrib attr =
		let fg,bg = set_attrib self attr in
		(match fg with None -> () | Some c -> self#set_colour c; colour <- c);
		(match bg with None -> () | Some c -> self#set_background c; background <- c);
	method get_width = width
	method get_height = height
	method set_x x =
		pen <- x, snd pen
	method get_x = 
		fst pen
	method move_x d = 
		pen <- fst pen + d, snd pen
	method set_y y = 
		pen <- fst pen, y
	method get_y = 
		snd pen
	method move_y d = 
		pen <- fst pen, snd pen + d
	method save =
		saved <- pen
	method restore =
		pen <- saved
	method saved_x = fst saved
	method saved_y = snd saved
	method set_saved p = saved <- p
end

let dummy_console =
	object
		inherit console as super
		val term = Asm.matrix8 0xB8000l 25 160
		val mutable width = 80
		val mutable height = 25
		method set_colour _ = ()
		method set_background _ = ()
		method erase count =
			let term = Bigarray.reshape_1 (Bigarray.genarray_of_array2 term) (width * height * 2) in
			let offset = super#get_x + width * super#get_y * 2 in
			for i = 0 to count - 1 do
				term.{i * 2 + offset} <- ' ';
				term.{i * 2 + offset + 1} <- char_of_int 7;
			done
		method draw ch = term.{super#get_y,super#get_x * 2} <- Char.chr ((UChar.int_of_uchar ch) land 0xFF); 1
		method update_cursor =
			if super#get_x >= width then begin
				super#set_x 0;
				super#set_y (super#get_y + 1);
			end;
			if super#get_y >= height then begin
				super#set_y (height - 1);
				(* should scroll display... *)
			end
	end

let current_console = ref dummy_console

(*let physical_console =
	let term = Asm.matrix8 0xB8000l 25 160 in {
		term = term;
		rows = Array2.dim1 term;
		cols = Array2.dim2 term / 2;
		curr_x = 0;
		curr_y = 0;
		save_x = -1;
		save_y = 0;
		attrib = 7;
	}

let erase console count =
	let term = reshape_1 (genarray_of_array2 console.term) (console.cols * console.rows * 2) in
	let offset = console.curr_x + console.cols * console.curr_y * 2 in
	for i = 0 to count - 1 do
		term.{i * 2 + offset} <- ' ';
		term.{i * 2 + offset + 1} <- char_of_int console.attrib;
	done*)



(*

ANSI VT100 terminal emulation is a state machine

http://en.wikipedia.org/wiki/ANSI_escape_code#Codes

ESC[m set attrib 0
ESC[s save cursor
ESC[u update to saved cursor
ESC[H move cursor home
ESC[J erase to end of screen
ESC[K erase to end of line

ESC[2J erase whole screen
ESC[<n>m set attrib n
ESC[<n>A move cursor up n lines or top
ESC[<n>B move cursor down n lines or bottom
ESC[<n>C move cursor right n cols or right
ESC[<n>D move cursor left n cols or left

ESC[<x>;<y>H move cursor to x,y or bottom right
ESC[<x>;<y>f ^^ (these are 1-based)
ESC[<m>;<n>m set attrib m, and n

ESC[<m>;<n>;<o>m set attrib m, n, and o

*)

let zero = int_of_char '0'

type result = Reset | Char | Cont of (console -> char -> result)

let
rec vt_init console = function
	| '\027' -> Cont vt_esc(* ESC *)
	| _ -> Char (* normal character *)
and vt_esc console = function 
	| '[' -> Cont vt_cmd (* ESC[ *)
	| _ -> Char (* invalid escape *)
and vt_cmd console = function
	| '0' .. '9' as ch -> (* ESC[<n>... *)
			Cont (vt_cmd_num (int_of_char ch - zero))
	| 'm' -> (* ESC[m *)
			console#attrib 0;
			Reset
	| 's' -> (* ESC[s *)
			console#save;
			Reset
	| 'u' -> (* ESC[u *)
			if console#saved_x >= 0 then begin
				console#restore;
				console#set_saved (-1, console#saved_y);
			end;
			Reset
	| 'H' -> (* ESC[H *)
			console#set_x 0;
			console#set_y 0;
			Reset
	| 'J' -> (* ESC[J *)
			console#erase ((console#get_height - console#get_y) * console#get_width - console#get_x);
			Reset
	| 'K' -> (* ESC[K *)
			console#erase (console#get_width - console#get_x);
			Reset
	| _ -> Char
and vt_cmd_num num console = function
	| '0' .. '9' as ch -> (* ESC[<n>... *)
			Cont (vt_cmd_num (num * 10 + (int_of_char ch - zero)))
	| ';' -> (* ESC[<n>;... *)
			Cont (vt_cmd_num_num num 0)
	| 'J' when num = 2 -> (* ESC[2J *)
			console#set_x 0;
			console#set_y 0;
			console#erase (console#get_height * console#get_width);
			Reset
	| 'm' -> (* ESC[<n>m *)
			console#attrib num;
			Reset
	| 'A' -> (* ESC[<n>A *)
			if num > console#get_y then
				console#set_y 0
			else
				console#move_y (-num);
			Reset
	| 'B' -> (* ESC[<n>B *)
			if num >= console#get_y + console#get_height then
				console#set_y (console#get_height - 1)
			else
				console#move_y num;
			Reset
	| 'C' -> (* ESC[<n>C *)
			if num >= console#get_x + console#get_width then
				console#set_x (console#get_width - 1)
			else
				console#move_x num;
			Reset
	| 'D' -> (* ESC[<n>D *)
			if num > console#get_x then
				console#set_x 0
			else
				console#move_x (-num);
			Reset
	| _ -> Char
and vt_cmd_num_num num1 num2 console = function
	| '0' .. '9' as ch ->
			Cont (vt_cmd_num_num num1 (num2 * 10 + (int_of_char ch - zero)))
	| ';' ->
			Cont (vt_cmd_num_num_num num1 num2 0)
	| 'H' | 'f' ->
			if num2 > console#get_width then
				console#set_x (console#get_width - 1)
			else
				console#set_x (num2 - 1);
			if num1 > console#get_height then
				console#set_y (console#get_height - 1)
			else
				console#set_y (num1 - 1);
			Reset
	| 'm' ->
			console#attrib num1;
			console#attrib num2;
			Reset
	| _ -> Char
and vt_cmd_num_num_num num1 num2 num3 console = function
	| '0' .. '9' as ch ->
			Cont (vt_cmd_num_num_num num1 num2 (num3 * 10 + (int_of_char ch - zero)))
	| 'm' ->
			console#attrib num1;
			console#attrib num2;
			console#attrib num3;
			Reset
	| _ -> Char

let vt_state = ref vt_init

let rec put console uchar =
	match UChar.int_of_uchar uchar with
		| 10 (* \n *) ->
				(* move down a line *)
				console#move_y 1;
				put console (UChar.uchar_of_int (Char.code '\r'))
		| 13 (* \r *) ->
				(* move to start of line *)
				console#set_x 0
		| 8 (* \b *) ->
				if console#get_x > 0 then begin
					console#move_x (-1);
					console#erase 1;
				end else if console#get_y > 0 then begin
					console#set_x (console#get_width - 1);
					console#move_y (-1);
					console#erase 1;
				end
		| 9 (* \t *) ->
				(* move to next tab stop *)
				console#move_x ((console#get_x + 5) mod 4)
		| _ ->
				(* display it -- handling double-width characters may cause some issues.... *)
				let offset = console#draw uchar in
				console#move_x offset
and process console uchar =
	let ucode = UChar.int_of_uchar uchar in
	if ucode < 256 then begin
		match !vt_state console (Char.chr ucode) with
			| Reset -> vt_state := vt_init
			| Char ->
					vt_state := vt_init;
					put console uchar;
					console#update_cursor
			| Cont k -> vt_state := k
	end else begin
		vt_state := vt_init;
		put console uchar;
		console#update_cursor
	end

(*let update_cursor console =
	if console.curr_x >= console.cols then begin
		console.curr_x <- console.curr_x mod console.cols;
		console.curr_y <- console.curr_y + 1
	end;
	if console.curr_y >= console.rows then begin
		console.curr_y <- console.rows - 1;
		(* scroll by one line *)
		Array2.blit
			(Array2.sub_left console.term 1 24)
			(Array2.sub_left console.term 0 24);
		let last_row = Array2.slice_left console.term 24 in
		for i = 0 to Array1.dim last_row / 2 - 1 do
			last_row.{i * 2} <- ' ';
			last_row.{i * 2 + 1} <- char_of_int console.attrib;
		done
	end;
	Asm.out8 0x3D4 0xE;
	Asm.out8 0x3D5 ((console.curr_y * console.cols + console.curr_x) asr 8);
	Asm.out8 0x3D4 0xF;
	Asm.out8 0x3D5 ((console.curr_y * console.cols + console.curr_x) land 0xFF)*)

let console_out = IO.from_out_channel (object
		method output buf ofs len =
			(* need to treat as possible utf-8 *)
			UTF8.iter (process !current_console) (String.sub buf ofs len);
			len
		method flush () = ()
		method close_out () = ()
	end)

let printf fmt = IO.printf console_out fmt

let dims () = !current_console#get_width,!current_console#get_height

(* redefined Pervasives.stdout at runtime :D *)
module StdOut = struct
	type t = unit
	
	open Vfs
	
	let (!) = Pervasives.(!)
	
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
	
	(* buffer bytes to try get UTF8 handling working... *)	
	let utf8_buffer = Buffer.create 1024
	let rec output_byte _ byte =
		Buffer.add_char utf8_buffer (Char.chr byte);
		try
			let s = Buffer.contents utf8_buffer in
			UTF8.validate s;
			output_bytes () s 0 (String.length s);
			Buffer.clear utf8_buffer
		with UTF8.Malformed_code -> ()
	and output_bytes _ buf ofs len =
		UTF8.iter (process !current_console) (String.sub buf ofs len); len
	
	let seek_out _ _ = raise Not_supported
	let pos_out _ = raise Not_supported
	let length_out _ = raise Not_supported
	
	let of_abstract_inode _ = ()
	let to_abstract_inode () = Obj.magic ()
end

let stdout' = { Vfs.ops = (module StdOut : Vfs.Inode); inode = Obj.magic () }

module type PERVASIVES = sig
	include module type of Pervasives
end

let pervasives = (module Pervasives : PERVASIVES)

let init () =
	(* runtime modification : stdout is field 23 in Pervasives *)
	let t = Obj.repr pervasives in
	Obj.set_field t 23 (Obj.repr stdout')
