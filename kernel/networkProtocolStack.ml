
(* The network protocol stack... *)

module Ethernet2 = struct

	type addr = Addr of int * int * int * int * int * int
	
	type t = { dst : addr; src : addr; protocol : int; content : Bitstring.t }
	
	let addr_printer () = function Addr (a,b,c,d,e,f) ->
		Printf.sprintf "%02X:%02X:%02X:%02X:%02X:%02X" a b c d e f
	
	let parse_addr bs = bitmatch bs with
		| { a : 8; b : 8; c : 8; d : 8; e : 8; f : 8 } ->
			Addr (a,b,c,d,e,f)
	
	let parse string =
		let bstring = Bitstring.bitstring_of_string string in
		bitmatch bstring with
		| {
			dst : 48 : bitstring;
			src : 48 : bitstring;
			protocol : 16;
			content : -1 : bitstring
		  } -> { dst = parse_addr dst; src = parse_addr src; protocol = protocol; content = content }

end

open IO
open IO.BigEndian

type token = Byte | Word | DWord | Bytes of int | Remainder

let rec (<->) a = function
	| b when a < b -> a :: (<->) (a + 1) b
	| b when a > b -> List.rev (b <-> a)
	| b -> [b]

let read_bytes i n =
	let rec read = function
		| 0 -> []
		| n -> read_byte i:: read (n-1)
	in
	List.rev (read n)

let write_bytes o = List.iter (write_byte o)

let rec pad_truncate list count acc =
	if count = 0 then List.rev acc
	else match list with
		| [] -> pad_truncate [] (count-1) (0 :: acc)
		| x :: xs -> pad_truncate xs (count-1) (x :: acc)

let pad_truncate list count = pad_truncate list count []	

let compose_gen format data : int list =
	let o = output_enum () in
	let compose = function
		| Byte, [b] -> write_byte o b
		| Word, [w] -> write_ui16 o w
		| DWord, [d] -> write_i32 o d
		| Bytes n, list -> write_bytes o (pad_truncate list n)
		| Remainder, list -> write_bytes o list
	in
	List.iter compose (List.combine format data);
	Obj.magic (ExtList.List.of_enum (close_out o))

let rec replace offset replacement list =
	match offset, replacement, list with
		| _, [], list -> list (* completed *)
		| 0, a::ax, _::bx -> a :: replace offset ax bx (* start replacing *)
		| n, _, b::bx when n > 0 -> b :: replace (offset-1) replacement bx
		| _ -> failwith "replace: invalid argument"

let checksum data =
	let rec checksum = function
		| a :: b :: rest -> ((a lsl 8) lor b) + checksum rest
		| [a] -> a
		| [] -> 0
	in
	let result = (lnot (checksum data mod 0xFFFF)) land 0xFFFF in
	[result asr 8; result land 0xFF]

let decompose_gen format data =
	let i = input_enum (Obj.magic (ExtList.List.enum data) : char Enum.t) in
	let rec decompose list = function
		| Byte -> [read_byte i]
		| Word -> [read_ui16 i]
		| DWord -> [read_i32 i]
		| Bytes 0 -> List.rev list
		| Bytes n -> decompose (read_byte i :: list) (Bytes (n-1))
		| Remainder -> try decompose (read_byte i :: list) Remainder with No_more_input -> List.rev list
	in
	List.map (decompose []) format

module IP = struct

	let format = [ Byte; Byte; Word; DWord; Byte; Byte; Word; Bytes 4; Bytes 4 ]
	
	type t = {
		version : int;
		tos : int;
		ttl : int;
		protocol : int;
		src : int list;
		dst : int list;
		data : int list;
	}
	
	let compose version tos ttl protocol src dst data =
		let header = compose_gen
			format
			[
				[ (version lsl 4) lor 5 ];
				[ tos ];
				[ List.length data + 20 ];
				[ 0 ];
				[ ttl ];
				[ protocol ];
				[ 0 ];
				src;
				dst;
			]
		in let checksum = checksum header
		in (replace 10 checksum header) @ data
	
	let decompose l =
		let [header_length] :: rest = decompose_gen [Byte; Remainder] l in
		let option_count = header_length land 0xFF - 5 in
		let options = List.map (fun _ -> Bytes 4) (1 <-> option_count)
		in decompose_gen (format @ options @ [Remainder]) l
		(* would be nice to make this return [t] *)
	
	let broadcast = [0xFF; 0xFF; 0xFF; 0xFF]

end

module UDP = struct

	let format = [ Word; Word; Word; Word; Remainder ]
	
	let compose src dst data =
		let packet = compose_gen
			format
			[
				[fst src];
				[fst dst];
				[List.length data + 8];
				[0];
				data
			]
		in let header = compose_gen
			[ Bytes 4; Bytes 4; Word; Word ]
			[
				snd src;
				snd dst;
				[List.length packet];
				[0x11];
			]
		in let checksum = checksum (header @ packet)
		in replace 6 checksum packet
	
	(* we don't have a decompose for UDP? interesting... *)

end

module Ethernet = struct

	let compose src dst protocol data =
		compose_gen
			[ Bytes 6; Bytes 6; Word; Remainder ]
			[
				dst;
				src;
				[protocol];
				data
			]
	
	let broadcast = [0xFF; 0xFF; 0xFF; 0xFF; 0xFF; 0xFF]

end
