
(* A different approach to packet parsing *)

(* We use bigarrays here! *)

open Bigarray

(* this is what we work on *)
type a = (int, int8_unsigned_elt, c_layout) Array1.t
type t = a * int

(* functions to read data from a bigarray, without the bigarray constraints *)

module R = struct
	external i32 : a -> int -> int -> int32 = "caml_ba_i32"
	external i16 : a -> int -> int -> int = "caml_ba_i16" "noalloc"
	external i8  : a -> int -> int -> int = "caml_ba_i8" "noalloc"
	(*let i16 a o1 o2 =
		let x = i8 a o1 o2 in
		let y = i8 a o1 (o2+1) in
		(x lsl 8) lor y*)
end

(* should provide functions to check that the packet meets an expected length *)

let to_eth_addr (a,b,c,d,e,f) = NetworkProtocolStack.Ethernet.Addr (a,b,c,d,e,f)
let to_ipv4_addr (a,b,c,d) = NetworkProtocolStack.IPv4.Addr (a,b,c,d)

module Ethernet = struct
	(* packet should be minimum of 14 bytes long *)
	let dst (t,o) =
		(R.i8 t o 0, R.i8 t o 1, R.i8 t o 2, R.i8 t o 3, R.i8 t o 4, R.i8 t o 5)
	
	let dst_addr (t,o) =
		NetworkProtocolStack.Ethernet.Addr
			(R.i8 t o 0, R.i8 t o 1, R.i8 t o 2, R.i8 t o 3, R.i8 t o 4, R.i8 t o 5)
	
	let src (t,o) =
		(R.i8 t o 6, R.i8 t o 7, R.i8 t o 8, R.i8 t o 9, R.i8 t o 10, R.i8 t o 11)
	
	let src_addr (t,o) =
		NetworkProtocolStack.Ethernet.Addr
			(R.i8 t o 6, R.i8 t o 7, R.i8 t o 8, R.i8 t o 9, R.i8 t o 10, R.i8 t o 11)
	
	let protocol (t,o) =
		R.i16 t o 12
	
	let content (t,o) = (t,o+14)
end

module IPv4 = struct
	(* packet is 5 dwords + options (header length-5) + (length - header length) dwords *)
	let tos (t,o) =
		R.i8 t o 1
	
	let ttl (t,o) =
		R.i8 t o 8
	
	let protocol (t,o) =
		R.i8 t o 9
	
	let src (t,o) =
		(R.i8 t o 12, R.i8 t o 13, R.i8 t o 14, R.i8 t o 15)
	
	let src_addr (t,o) =
		NetworkProtocolStack.IPv4.Addr
			(R.i8 t o 12, R.i8 t o 13, R.i8 t o 14, R.i8 t o 15)
	
	let dst (t,o) =
		(R.i8 t o 16, R.i8 t o 17, R.i8 t o 18, R.i8 t o 19)
	
	let dst (t,o) =
		NetworkProtocolStack.IPv4.Addr
			(R.i8 t o 16, R.i8 t o 17, R.i8 t o 18, R.i8 t o 19)
	
	let content (t,o) =
		(* the offset here depends on the header length *)
		if R.i8 t o 0 <> 0x45 then
			failwith "ipv4: contains options";
		(t,o+20)
	
	let content_length (t,o) =
		R.i16 t o 2 - 20 (* content above would have failed if header size not 20 bytes *)
		
end

module UDP = struct
	let src (t,o) =
		R.i16 t o 0
	
	let dst (t,o) =
		R.i16 t o 2
	
	let content (t,o) = (t,o+8)
end

module TCP = struct
	let src (t,o) =
		R.i16 t o 0
	
	let dst (t,o) =
		R.i16 t o 2
	
	let seq (t,o) =
		R.i32 t o 4
	
	let ack (t,o) =
		R.i32 t o 8
	
	let flags (t,o) =
		(R.i8 t o 13) land 0x3F
	
	let window (t,o) =
		R.i16 t o 14
	
	let content (t,o) =
		if (R.i8 t o 12) lsr 4 <> 5 then
			failwith "tcp: header length <> 5";
		(t,o+20)
	
	let content_length (t,o) packet_length =
		(* content above would have failed if header length not 20 bytes *)
		packet_length - 20
end

module ARP = struct
	let is_ipv4_over_ethernet (t,o) =
		R.i16 t o 0 = 0x0001 && R.i16 t o 2 = 0x0800
	
	let hw_type (t,o) =
		R.i16 t o 0
	
	let proto_type (t,o) =
		R.i16 t o 2
	
	let opcode (t,o) =
		R.i16 t o 6
	
	let sender_eth (t,o) =
		R.i8 t o 8, R.i8 t o 9, R.i8 t o 10, R.i8 t o 11, R.i8 t o 12, R.i8 t o 13
	
	let sender_ip (t,o) =
		R.i8 t o 14, R.i8 t o 15, R.i8 t o 16, R.i8 t o 17
	
	let target_eth (t,o) =
		R.i8 t o 18, R.i8 t o 19, R.i8 t o 20, R.i8 t o 21, R.i8 t o 22, R.i8 t o 23
	
	let target_ip (t,o) =
		R.i8 t o 24, R.i8 t o 25, R.i8 t o 26, R.i8 t o 27
end
