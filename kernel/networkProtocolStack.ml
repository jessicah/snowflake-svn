
(* The network protocol stack... *)

let word bstring n =
	bitmatch bstring with
	| { word : n : bigendian } when word <= 0xFFFF_L -> Int64.to_int word
	| { _ } -> assert(false)

let checksum data =
	let rec checksum rem sum data =
		if rem = 0 then
			(lnot (sum mod 0xFFFF)) land 0xFFFF
		else if rem < 16 then
			(lnot ((sum + word data rem) mod 0xFFFF)) land 0xFFFF
		else
			checksum (rem-16) (word data 16 + sum) (Bitstring.dropbits 16 data)
	in checksum (Bitstring.bitstring_length data) 0 data

module Ethernet = struct

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
	
	let unparse_addr = function Addr (a,b,c,d,e,f) ->
		BITSTRING {
			a : 8; b : 8; c : 8; d : 8; e : 8; f : 8
		}
	
	let unparse t =
		BITSTRING {
			unparse_addr t.dst : 48 : bitstring;
			unparse_addr t.src : 48 : bitstring;
			t.protocol : 16;
			t.content : -1 : bitstring
		}
	
	let broadcast = Addr (0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF)

end

module IPv4 = struct
	
	type addr = Addr of int * int * int * int
	
	type t = {
		tos : int;
		ttl : int;
		protocol : int;
		src : addr;
		dst : addr;
		options : Bitstring.t;
		content : Bitstring.t;
	}
	
	let addr_printer () = function Addr (a,b,c,d) ->
		Printf.sprintf "%d.%d.%d.%d" a b c d
	
	let parse_addr bs = bitmatch bs with
		| { a : 8; b : 8; c : 8; d : 8 } ->
			Addr (a,b,c,d)
	
	let parse string =
		let bstring = Bitstring.bitstring_of_string string in
		bitmatch bstring with
		| { version : 4; hdrlen : 4; tos : 8; length : 16;
			identification : 16; flags : 3; fragoffset : 13;
			ttl : 8; protocol : 8; checksum : 16;
			source : 32 : bitstring;
			dest : 32 : bitstring;
			options : (hdrlen-5) * 32 : bitstring;
			payload : (length - hdrlen*4) * 8 : bitstring } as packet
			when version = 4 -> (* match an IPv4 packet *)
			{
				tos = tos;
				(*length = length;
				identification = identification;
				flags = flags;
				fragoffset = fragoffset;*)
				ttl = ttl;
				protocol = protocol;
				(* should verify checksum... *)
				src = parse_addr source;
				dst = parse_addr dest;
				options = options;
				content = payload;
			}
		| { version : 4 } -> failwith "Expected IPv4 packet"
		| { _ } -> failwith "Not an IPv4 packet"
	
	let unparse_addr = function Addr (a,b,c,d) ->
		BITSTRING { a : 8; b : 8; c : 8; d : 8 }
	
	let unparse t =
		let hdrlen = (Bitstring.bitstring_length t.options) / 32 + 5 in
		let length = Bitstring.bitstring_length t.content in
		let packet = BITSTRING {
			4 : 4; hdrlen : 4;
			t.tos : 8; length : 16;
			0 (* identification *) : 16; 0 (* flags *) : 3;
			0 (* fragoffset *) : 13;
			t.ttl : 8; t.protocol : 8; 0 (* checksum *) : 16;
			unparse_addr t.src : 32 : bitstring;
			unparse_addr t.dst : 32 : bitstring;
			t.options : Bitstring.bitstring_length t.options : bitstring;
			t.content : -1 : bitstring
		} in
		let checksum_field = Bitstring.subbitstring packet 80 16 in
		let n = checksum (Bitstring.subbitstring packet 0 80) in
		let checksum = BITSTRING { n : 16 } in
		Bitstring.blit checksum checksum_field;
		packet
	
	let broadcast = Addr (255, 255, 255, 255)

end

(*
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
*)
