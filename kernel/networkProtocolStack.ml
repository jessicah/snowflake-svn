
(* The network protocol stack... *)

let word bstring n =
	bitmatch bstring with
	| { word : n : bigendian } when word <= 0xFFFF_L -> Int64.to_int word
	| { _ : -1 : bitstring } -> assert(false)

let checksum data =
	let rec checksum rem sum data =
		if rem = 0 then
			(lnot (sum mod 0xFFFF)) land 0xFFFF
		else if rem = 8 then
			(lnot ((sum + word data rem) mod 0xFFFF)) land 0xFFFF
		else
			checksum (rem-16) (word data 16 + sum) (Bitstring.dropbits 16 data)
	in checksum (Bitstring.bitstring_length data) 0 data

(* some generic parse/unparse functions *)

let parse_string = Bitstring.string_of_bitstring
let unparse_string = Bitstring.bitstring_of_string

let parse_array bits =
	let len = Bitstring.bitstring_length bits in
	if len mod 8 <> 0 then failwith "bitstring must be byte-aligned";
	let array = Array.make (len / 8) 0 in
	let rec parse_bytes i bits = bitmatch bits with
		| { byte : 8; rest : -1 : bitstring } ->
			array.(i) <- byte;
			parse_bytes (i+1) rest
		| { rest : -1 : bitstring }
			when Bitstring.bitstring_length rest = 0 ->
			array
		| { _ : -1 : bitstring } -> assert false (* the test at start avoids this case *)
	in parse_bytes 0 bits

let unparse_array array =
	let bits = Array.fold_right (fun x y -> Bitstring.make_bitstring 8 (Char.chr x) :: y) array [] in
	Bitstring.concat bits

let rec parse_list bits acc = bitmatch bits with
	| { byte : 8; rest : -1 : bitstring } ->
		parse_list rest (byte :: acc)
	| { rest : -1 : bitstring }
		when Bitstring.bitstring_length rest = 0 ->
		acc
	| { _ : -1 : bitstring } -> assert false

let parse_list bits =
	if Bitstring.bitstring_length bits mod 8 <> 0 then
		failwith "bitstring must be byte aligned";
	parse_list bits []

let unparse_list list =
	let bits = List.map (fun i -> Bitstring.make_bitstring 8 (Char.chr i)) list in
	Bitstring.concat bits
	
module Ethernet = struct

	type addr = Addr of int * int * int * int * int * int
	
	type t = { dst : addr; src : addr; protocol : int; content : Bitstring.t }
	
	let addr_printer () = function Addr (a,b,c,d,e,f) ->
		Printf.sprintf "%02X:%02X:%02X:%02X:%02X:%02X" a b c d e f
	
	let parse_addr bs = bitmatch bs with
		| { a : 8; b : 8; c : 8; d : 8; e : 8; f : 8 } ->
			Addr (a,b,c,d,e,f)
	
	let parse bstring =
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
	
	let invalid = Addr (0, 0, 0, 0, 0, 0)
	
	let to_string = function
		Addr(a,b,c,d,e,f) ->
			Printf.sprintf "%02x:%02x:%02x:%02x:%02x:%02x" a b c d e f

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
	
	let parse bstring =
		bitmatch bstring with
		| { version : 4; hdrlen : 4; tos : 8; length : 16;
			identification : 16; flags : 3; fragoffset : 13;
			ttl : 8; protocol : 8; checksum : 16;
			source : 32 : bitstring;
			dest : 32 : bitstring;
			options : (hdrlen-5) * 32 : bitstring;
			payload : (length - hdrlen*4) * 8 : bitstring } (*as packet*)
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
		| { _ : -1 : bitstring } -> failwith "Not an IPv4 packet"
	
	let unparse_addr = function Addr (a,b,c,d) ->
		BITSTRING { a : 8; b : 8; c : 8; d : 8 }
	
	let unparse t =
		let hdrlen = (Bitstring.bitstring_length t.options) / 32 + 5 in
		let length = Bitstring.bitstring_length t.content / 8 in
		let packet = BITSTRING {
			4 : 4; hdrlen : 4;
			t.tos : 8; length + hdrlen*4 : 16;
			0 (* identification *) : 16; 0 (* flags *) : 3;
			0 (* fragoffset *) : 13;
			t.ttl : 8; t.protocol : 8; 0 (* checksum *) : 16;
			unparse_addr t.src : 32 : bitstring;
			unparse_addr t.dst : 32 : bitstring;
			t.options : Bitstring.bitstring_length t.options : bitstring(*;
			t.content : -1 : bitstring*)
		} in
		let checksum_field = Bitstring.subbitstring packet 80 16 in
		let n = checksum packet in
		let checksum = BITSTRING { n : 16 } in
		Bitstring.blit checksum checksum_field;
		Bitstring.concat [packet; t.content]
	
	let broadcast = Addr (255, 255, 255, 255)
	let invalid = Addr (0, 0, 0, 0)
	
	let to_string = function
		Addr(a,b,c,d) ->
			Printf.sprintf "%d.%d.%d.%d" a b c d

end

module UDP = struct

	type t = {
		src: int;
		dst: int;
		content : Bitstring.t;
	}
	
	let parse bstring =
		bitmatch bstring with
		| { src : 16; dst : 16;
			length : 16; checksum : 16;
			content : (length-8) * 8 : bitstring
		  } -> {
			src = src;
			dst = dst;
			content = content;
		} (* validate checksum later *)
	
	let unparse t src_addr dst_addr =
		let packet = BITSTRING {
			t.src : 16; t.dst : 16;
			((Bitstring.bitstring_length t.content)/8) + 8 : 16;
			0 (* checksum *) : 16;
			t.content : -1 : bitstring }
		in
		let header = BITSTRING {
			IPv4.unparse_addr src_addr : 32 : bitstring;
			IPv4.unparse_addr dst_addr : 32 : bitstring;
			Bitstring.bitstring_length packet / 8 : 16;
			0x11 : 16 }
		in
		let checksum_field = Bitstring.subbitstring packet 48 16 in
		let n = checksum (Bitstring.concat [header; packet]) in
		let checksum = BITSTRING { n : 16 } in
		Bitstring.blit checksum checksum_field;
		packet

end

module TCP = struct

	type flags = Urgent | Ack | Push | Reset | Syn | Finish
	
	let flags = [ 32, Urgent; 16, Ack; 8, Push; 4, Reset; 2, Syn; 1, Finish ]
	
	let of_flag f = List.assoc f (List.map (fun (a,b) -> (b,a)) flags)
	
	let to_flags f =
		List.map snd (List.filter (fun p -> fst p land f <> 0) flags)
	
	type t = {
		src_port : int;
		dst_port : int;
		seq_num : int32;
		ack_num : int32;
		flags : flags list;
		window : int;
		content : Bitstring.t;
	}
	
	let parse bits =
		bitmatch bits with
		{
			src_port : 16;
			dst_port : 16;
			seq_num : 32;
			ack_num : 32;
			offset : 4;
			_ : 6; (* reserved *)
			flags : 6;
			window : 16;
			_ : 16; (* checksum *)
			_ : 16; (* urgent pointer *)
			_ : (offset-5) * 8 : bitstring;
			content : -1 : bitstring
		} -> {
				src_port = src_port;
				dst_port = dst_port;
				seq_num = seq_num;
				ack_num = ack_num;
				flags = to_flags flags;
				window = window;
				content = content;
			}
	
	let unparse t src_ip dst_ip (* no options or urgent pointer *) =
		let flags = List.fold_right (fun f x -> of_flag f lor x) t.flags 0 in
		let packet = BITSTRING {
			t.src_port : 16;
			t.dst_port : 16;
			t.seq_num : 32;
			t.ack_num : 32;
			0x6000 lor flags : 16; (* no options, offset to data *)
			t.window : 16;
			0 : 16; (* checksum *)
			0 : 16; (* urgent pointer *)
			0l : 32; (* padding I think *)
			t.content : -1 : bitstring
		} in
		(* put the checksum in *)
		let checksum_field = Bitstring.subbitstring packet 128 16 in
		let len = Bitstring.bitstring_length packet / 8 in
		let header = BITSTRING {
			IPv4.unparse_addr src_ip : 32 : bitstring;
			IPv4.unparse_addr dst_ip : 32 : bitstring;
			0x0006 : 16; (* tcp protocol *)
			len : 16
		} in
		let checksum_data = Bitstring.concat [
				header;
				packet;
				if len mod 2 = 0 then Bitstring.empty_bitstring
				else BITSTRING { 0 : 8 }
			]
		in
		let n = checksum checksum_data in
		Bitstring.blit (BITSTRING { n : 16 }) checksum_field;
		packet
end
