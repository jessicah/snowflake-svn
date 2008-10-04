
(* Keyboard Driver *)

let test_bit w b = (w asr b) land 1 = 1

let test_input_full w = test_bit w 1
let test_mouse_output_full w = test_bit w 5
let test_keyboard_output_full w = test_bit w 0

let test_output_full w =
	test_keyboard_output_full w && not (test_mouse_output_full w)

let status () = Asm.in8 0x64
let data () = Asm.in8 0x60

let rec wait () =
	let w = status () in
	if not (test_input_full w) then ()
	else wait ()

let controller w =
	wait ();
	Asm.out8 0x64 w

let enable () =
	wait ();
	Asm.out8 0x60 0xF4;
	ignore (status ());
	controller 0xAE

let caps  = ref false
let shift = ref false

let nul = '\000'

let qwerty = [|
		[| nul;nul;'1';'2';'3';'4';'5';'6';'7';'8';'9';'0';'-';'=';nul;nul;'q';'w';'e';'r';'t';'y';'u';'i';'o';'p';'[';']';nul;nul;'a';'s';'d';'f';'g';'h';'j';'k';'l';';';'\'';'`';nul;'\\';'z';'x';'c';'v';'b';'n';'m';',';'.';'/';nul;nul;nul;' '|];
		[|nul;nul;'!';'@';'#';'$';'%';'^';'&';'*';'(';')';'_';'+';nul;nul;'Q';'W';'E';'R';'T';'Y';'U';'I';'O';'P';'{';'}';nul;nul;'A';'S';'D';'F';'G';'H';'J';'K';'L';':';'"'; '~';nul;'|' ;'Z';'X';'C';'V';'B';'N';'M';'<';'>';'?';nul;nul;nul;' '|];
		[|nul;nul;'1';'2';'3';'4';'5';'6';'7';'8';'9';'0';'-';'=';nul;nul;'Q';'W';'E';'R';'T';'Y';'U';'I';'O';'P';'[';']';nul;nul;'A';'S';'D';'F';'G';'H';'J';'K';'L';';';'\'';'`';nul;'|' ;'Z';'X';'C';'V';'B';'N';'M';',';'.';'/';nul;nul;nul;' '|];
		[|nul;nul;'!';'@';'#';'$';'%';'^';'&';'*';'(';')';'_';'+';nul;nul;'q';'w';'e';'r';'t';'y';'u';'i';'o';'p';'{';'}';nul;nul;'a';'s';'d';'f';'g';'h';'j';'k';'l';':';'"'; '~';nul;'\\';'z';'x';'c';'v';'b';'n';'m';'<';'>';'?';nul;nul;nul;' '|];
	|]

let keymap = ref qwerty

let set_keymap map = keymap := map

let buffer = Queue.create ()

let m = Mutex.create ()
let cv = Condition.create ()

let add ch =
	Mutex.lock m;
	Queue.add ch buffer;
	(* Now something to read, signal any waiting thread *)
	Condition.signal cv;
	Mutex.unlock m

let read () =
	match data () with
		| 0x2A | 0x36 -> shift := true
		| 0xAA | 0xB6 -> shift := false
		| 0x0F -> add '\t'
		| 0x1C -> add '\n'
		| s when s > 0x80 -> ()
		| s ->
			let ch = try
				if !shift then
					if !caps then !keymap.(2).(s)
					else !keymap.(1).(s)
				else if !caps then !keymap.(3).(s)
				else !keymap.(0).(s)
			with _ -> nul in
			if ch <> nul then add ch

let rec get_char () =
	Mutex.lock m;
	while Queue.is_empty buffer do
		Condition.wait cv m
	done;
	let result = Queue.pop buffer in
	Mutex.unlock m;
	result

let init () =
	enable ();
	read ();
	ignore (Sys.signal 1 (Sys.Signal_handle (fun _ -> read ())))
