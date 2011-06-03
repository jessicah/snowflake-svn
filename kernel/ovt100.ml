
class virtual console = object
	val mutable pen = 0, 0
	val mutable saved = 0, 0
	val mutable virtual width : int
	val mutable virtual height : int
	method virtual attrib : int -> unit
	method virtual erase : int -> unit
	method virtual draw : UChar.uchar -> int
	method virtual update_cursor : unit
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
		inherit console
		val mutable width = 1
		val mutable height = 1
		method attrib _ = ()
		method erase _ = ()
		method draw _ = 0
		method update_cursor = ()
	end

let current_console = ref dummy_console

(*type console = {
		term : (char, int8_unsigned_elt, c_layout) Array2.t;
		rows : int;
		cols : int;
		mutable curr_x : int;
		mutable curr_y : int;
		mutable save_x : int;
		mutable save_y : int;
		mutable attrib : int;
	}*)

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
	foreground 30-37; background 40-47
	bright send ESC[1;<n>m
					black		red		green		yellow	blue	magenta		cyan	white
	colour   30      31     32      33     34     35       36     37
*)
(*let set_attrib console attr =
	let ansi_to_vga = [| 0; 4; 2; 6; 1; 5; 3; 7 |] in
	console.attrib <- match attr with
		| 0 -> 7
		| 1 -> console.attrib lor 0x08
		| 7 -> (console.attrib land 0x88) lor ((console.attrib land 0x07) lsl 4) lor ((console.attrib land 0x70) asr 4)
		| n when n >= 30 && n <= 37 ->
				(console.attrib land (lnot 0x07)) lor ansi_to_vga.(n - 30)
		| n when n >= 40 && n <= 47 ->
				(console.attrib land (lnot 0x70)) lor (ansi_to_vga.(n - 40) lsl 4)
		| n -> console.attrib*)

(* ANSI VT100 terminal emulation is a state machine *)

(*

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
			(***set_attrib console 0;***)
			console#attrib 0;
			Reset
	| 's' -> (* ESC[s *)
			(***console.save_x <- console.curr_x;
			console.save_y <- console.curr_y;***)
			console#save;
			Reset
	| 'u' -> (* ESC[u *)
			(***if console.save_x >= 0 then begin
				console.curr_x <- console.save_x;
				console.curr_y <- console.save_y;
				console.save_x <- -1;
			end;***)
			if console#saved_x >= 0 then begin
				console#restore;
				console#set_saved (-1, console#saved_y);
			end;
			Reset
	| 'H' -> (* ESC[H *)
			(***console.curr_x <- 0;
			console.curr_y <- 0;***)
			console#set_x 0;
			console#set_y 0;
			Reset
	| 'J' -> (* ESC[J *)
			(***erase console ((console.rows - console.curr_y) * console.cols - console.curr_x);***)
			console#erase ((console#get_height - console#get_y) * console#get_width - console#get_x);
			Reset
	| 'K' -> (* ESC[K *)
			(***erase console (console.cols - console.curr_x);***)
			console#erase (console#get_height - console#get_x);
			Reset
	| _ -> Char
and vt_cmd_num num console = function
	| '0' .. '9' as ch -> (* ESC[<n>... *)
			Cont (vt_cmd_num (num * 10 + (int_of_char ch - zero)))
	| ';' -> (* ESC[<n>;... *)
			Cont (vt_cmd_num_num num 0)
	| 'J' when num = 2 -> (* ESC[2J *)
			(***console.curr_x <- 0;
			console.curr_y <- 0;
			erase console (console.rows * console.cols);***)
			console#set_x 0;
			console#set_y 0;
			console#erase (console#get_height * console#get_width);
			Reset
	| 'm' -> (* ESC[<n>m *)
			(***set_attrib console num;***)
			console#attrib num;
			Reset
	| 'A' -> (* ESC[<n>A *)
			(***if num > console.curr_y then
				console.curr_y <- 0
			else
				console.curr_y <- console.curr_y - num;***)
			if num > console#get_y then
				console#set_y 0
			else
				console#move_y (-num);
			Reset
	| 'B' -> (* ESC[<n>B *)
			(***if num >= console.curr_y + console.rows then
				console.curr_y <- console.rows - 1
			else
				console.curr_y <- console.curr_y + num;***)
			if num >= console#get_y + console#get_height then
				console#set_y (console#get_height - 1)
			else
				console#move_y num;
			Reset
	| 'C' -> (* ESC[<n>C *)
			(*if num >= console.curr_x + console.cols then
				console.curr_x <- console.cols - 1
			else
				console.curr_x <- console.curr_x + num;*)
			if num >= console#get_x + console#get_width then
				console#set_x (console#get_width - 1)
			else
				console#move_x num;
			Reset
	| 'D' -> (* ESC[<n>D *)
			(*if num > console.curr_x then
				console.curr_x <- 0
			else
				console.curr_x <- console.curr_x - num;*)
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
			(*if num2 > console.cols then
				console.curr_x <- console.cols - 1
			else
				console.curr_x <- num2 - 1;
			if num1 > console.rows then
				console.curr_y <- console.rows - 1
			else
				console.curr_y <- num1 - 1;*)
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
			(*set_attrib console num1;
			set_attrib console num2;*)
			console#attrib num1;
			console#attrib num2;
			Reset
	| _ -> Char
and vt_cmd_num_num_num num1 num2 num3 console = function
	| '0' .. '9' as ch ->
			Cont (vt_cmd_num_num_num num1 num2 (num3 * 10 + (int_of_char ch - zero)))
	| 'm' ->
			(****set_attrib console num1;
			set_attrib console num2;
			set_attrib console num3;**)
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
			| Char -> put console uchar; console#update_cursor
			| Cont k -> vt_state := k
	end else begin
		put console uchar; console#update_cursor
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
	Asm.out8 0x3D5 ((console.curr_y * console.cols + console.curr_x) land 0xFF)

let process_char console ch = match !vt_state console ch with
	| Reset -> vt_state := vt_init
	| Char -> put_char console ch; update_cursor console
	| Cont k -> vt_state := k*)

(*let console_out = from_out_chars (object
		method put ch = process_char physical_console ch
		method flush () = ()
		method close_out () = ()
	end)

let () = printf console_out "\027[2J" (* clear the console *)

let printf fmt = printf console_out fmt
*)

let console_out = IO.from_out_chars (object
		method put ch = process !current_console (UChar.uchar_of_int (Char.code ch))
		method flush () = ()
		method close_out () = ()
	end)

let console_out = IO.from_out_channel (object
		method output buf ofs len =
			(* need to treat as possible utf-8 *)
			UTF8.iter (process !current_console) (String.sub buf ofs len);
			len
		method flush () = ()
		method close_out () = ()
	end)

let printf fmt = IO.printf console_out fmt
