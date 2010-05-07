
(* Time *)

let read addr =
	Asm.out8 0x70 addr;
	Asm.in8 0x71

let write addr value =
	Asm.out8 0x70 addr;
	Asm.out8 0x71 value

module R = struct
	let second = 0x00
	let minute = 0x02
	let hour = 0x04
	let weekday = 0x06
	let day = 0x07
	let month = 0x08
	let year = 0x09
	let century = 0x32
end

let dec_of_bcd n =
	let digit x = if n land x <> 0 then x else 0 in
	(digit 8) + (digit 4) + (digit 2) + (digit 1)

let int_of_bcd n =
	let i = ref [] in
	let n = ref n in
	while !n <> 0 do
		i := (dec_of_bcd (!n land 15)) :: !i;
		n := !n asr 4;
	done;
	List.fold_left (fun a b -> a * 10 + b) 0 !i

let time () =
	int_of_bcd (read R.hour),
	int_of_bcd (read R.minute),
	int_of_bcd (read R.second)

let date () =
	int_of_bcd (read R.weekday),
	int_of_bcd (read R.day),
	int_of_bcd (read R.month),
	(int_of_bcd (read R.year)) + (int_of_bcd (read R.century)) * 100

let weekday = function
| 0 -> "Sunday"
| 1 -> "Monday"
| 2 -> "Tuesday"
| 3 -> "Wednesday"
| 4 -> "Thursday"
| 5 -> "Friday"
| 6 -> "Saturday"
| _ -> assert false

let month = function
| 1 -> "January"
| 2 -> "February"
| 3 -> "March"
| 4 -> "April"
| 5 -> "May"
| 6 -> "June"
| 7 -> "July"
| 8 -> "August"
| 9 -> "September"
| 10 -> "October"
| 11 -> "November"
| 12 -> "December"
| _ -> assert false

let print_time () =
	let h,m,s = time () in
	Printf.sprintf "%02d:%02d:%02d %s" (if h > 12 then h - 12 else h) m s (if h < 12 then "AM" else "PM")

let print_date () =
	let w,d,m,y = date () in
	Printf.sprintf "%s, %d %s %d" (weekday w) d (month m) y
