
open Bigarray
open IO

type console = {
		term : (char, int8_unsigned_elt, c_layout) Array2.t;
		rows : int;
		cols : int;
		mutable curr_x : int;
		mutable curr_y : int;
		mutable save_x : int;
		mutable save_y : int;
		mutable attrib : int;
	}

let physical_console =
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
	let offset = console.curr_x * console.cols + console.curr_y in
	for i = 0 to count - 1 do
		term.{i * 2 + offset} <- ' ';
		term.{i * 2 + offset + 1} <- char_of_int console.attrib;
	done

let set_attrib console attr =
	let ansi_to_vga = [| 0; 4; 2; 6; 1; 5; 3; 7 |] in
	console.attrib <- match attr with
		| 0 -> 7
		| 1 -> console.attrib lor 0x08
		| 7 -> (console.attrib land 0x88) lor ((console.attrib land 0x07) lsl 4) lor ((console.attrib land 0x70) asr 4)
		| n when n >= 30 && n <= 37 ->
				(console.attrib land (lnot 0x07)) lor ansi_to_vga.(n - 30)
		| n when n >= 40 && n <= 47 ->
				(console.attrib land (lnot 0x70)) lor (ansi_to_vga.(n - 40) lsl 4)
		| n -> console.attrib

(* ANSI VT100 terminal emulation is a state machine *)

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
			set_attrib console 0;
			Reset
	| 's' -> (* ESC[s *)
			console.save_x <- console.curr_x;
			console.save_y <- console.curr_y;
			Reset
	| 'u' -> (* ESC[u *)
			if console.save_x >= 0 then begin
				console.curr_x <- console.save_x;
				console.curr_y <- console.save_y;
				console.save_x <- -1;
			end;
			Reset
	| 'H' -> (* ESC[H *)
			console.curr_x <- 0;
			console.curr_y <- 0;
			Reset
	| 'J' -> (* ESC[J *)
			erase console ((console.rows - console.curr_y) * console.cols - console.curr_x);
			Reset
	| 'K' -> (* ESC[K *)
			erase console (console.cols - console.curr_x);
			Reset
	| _ -> Char
and vt_cmd_num num console = function
	| '0' .. '9' as ch -> (* ESC[<n>... *)
			Cont (vt_cmd_num (num * 10 + (int_of_char ch - zero)))
	| ';' -> (* ESC[<n>;... *)
			Cont (vt_cmd_num_num num 0)
	| 'J' when num = 2 -> (* ESC[2J *)
			console.curr_x <- 0;
			console.curr_y <- 0;
			erase console (console.rows * console.cols);
			Reset
	| 'm' -> (* ESC[<n>m *)
			set_attrib console num;
			Reset
	| 'A' -> (* ESC[<n>A *)
			if num > console.curr_y then
				console.curr_y <- 0
			else
				console.curr_y <- console.curr_y - num;
			Reset
	| 'B' -> (* ESC[<n>B *)
			if num >= console.curr_y + console.rows then
				console.curr_y <- console.rows - 1
			else
				console.curr_y <- console.curr_y + num;
			Reset
	| 'C' -> (* ESC[<n>C *)
			if num >= console.curr_x + console.cols then
				console.curr_x <- console.cols - 1
			else
				console.curr_x <- console.curr_x + num;
			Reset
	| 'D' -> (* ESC[<n>D *)
			if num > console.curr_x then
				console.curr_x <- 0
			else
				console.curr_x <- console.curr_x - num;
			Reset
	| _ -> Char
and vt_cmd_num_num num1 num2 console = function
	| '0' .. '9' as ch ->
			Cont (vt_cmd_num_num num1 (num2 * 10 + (int_of_char ch - zero)))
	| ';' ->
			Cont (vt_cmd_num_num_num num1 num2 0)
	| 'H' | 'f' ->
			if num2 > console.cols then
				console.curr_x <- console.cols - 1
			else
				console.curr_x <- num2 - 1;
			if num1 > console.rows then
				console.curr_y <- console.rows - 1
			else
				console.curr_y <- num1 - 1;
			Reset
	| 'm' ->
			set_attrib console num1;
			set_attrib console num2;
			Reset
	| _ -> Char
and vt_cmd_num_num_num num1 num2 num3 console = function
	| '0' .. '9' as ch ->
			Cont (vt_cmd_num_num_num num1 num2 (num3 * 10 + (int_of_char ch - zero)))
	| 'm' ->
			set_attrib console num1;
			set_attrib console num2;
			set_attrib console num3;
			Reset
	| _ -> Char

let vt_state = ref vt_init

let rec put_char console ch =
	Asm.out8 0x3F8 (Char.code ch);
	match ch with
	| '\n' ->
			(* move down a line *)
			console.curr_y <- console.curr_y + 1;
			put_char console '\r'
	| '\r' ->
			(* move to start of line *)
			console.curr_x <- 0
	| '\b' ->
			(* erase last character, and update cursor *)
			if console.curr_x > 0 then begin
				console.curr_x <- console.curr_x - 1;
				console.term.{console.curr_y, console.curr_x * 2} <- ' '
			end else if console.curr_y > 0 then begin
				console.curr_x <- console.cols - 1;
				console.curr_y <- console.curr_y - 1;
				console.term.{console.curr_y, console.curr_x * 2} <- ' '
			end
	| '\t' ->
			(* move to next tab stop (every 4 chars) *)
			console.curr_x <- (console.curr_x + 4) mod 4
	| ch ->
			(* display it :-) *)
			console.term.{console.curr_y, console.curr_x * 2} <- ch;
			console.term.{console.curr_y, console.curr_x * 2 + 1} <- (char_of_int console.attrib);
			console.curr_x <- console.curr_x + 1

let update_cursor console =
	if console.curr_x >= console.cols then begin
		console.curr_x <- console.curr_x mod console.cols;
		console.curr_y <- console.curr_y + 1
	end;
	if console.curr_y >= console.rows then begin
		console.curr_y <- console.rows - 1;
		(* scroll by one line *)
	end;
	Asm.out8 0x3D4 0xE;
	Asm.out8 0x3D5 ((console.curr_y * console.cols + console.curr_x) asr 8);
	Asm.out8 0x3D4 0xF;
	Asm.out8 0x3D5 ((console.curr_y * console.cols + console.curr_x) land 0xFF)

let process_char console ch = match !vt_state console ch with
	| Reset -> vt_state := vt_init
	| Char -> put_char console ch; update_cursor console
	| Cont k -> vt_state := k

let console_out = from_out_chars (object
		method put ch = process_char physical_console ch
		method flush () = ()
		method close_out () = ()
	end)

let () = printf console_out "\027[2J" (* clear the console *)

let printf fmt = printf console_out fmt
