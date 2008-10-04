
open Parser

type packet
	= RRQ of string * string
	| WRQ of string * string
	| Data of int * int array
	| Ack of int
	| Error of int * string

let char = char_of_int >>= word8

let rec loop cond p = P begin fun input ->
		let Out (x, input) = parse p input in
		if cond x then
			let Out (xs, input) = parse (loop cond p) input in
			Out (x :: xs, input)
		else
			Out ([], input)
	end

let string =
	let charList = loop ((<>) '\000') char in
	ExtString.String.implode >>= charList

let packet = P begin fun input ->
		let Out (opcode, input) = parse word16 input in
		match opcode with
			| 1 -> (fun x y -> RRQ (x, y)) >>= string >> string
			| 2 -> (fun x y -> WRQ (x, y)) >>= string >> string
			| 3 -> (fun x y -> Data (x, y)) >>= word16 >> string
			| 4 -> (fun x -> Ack x) >>= word16
			| 5 -> (fun x y -> Error (x, y)) >>= word16 >> string
			| _ -> assert false
	end
