
(*
	Currently NetworkProtocolStack is responsible for
	constructing packets to be sent. Sending appears
	to work fine at present.
	
	And PacketParsing is responsible for taking the
	received input and parsing into different kinds
	of packets. References a bigarray. Receiving is
	very much slow and broken, and gets worse over
	time. Bigarrays aren't GC friendly either.
*)

(*
	Stage One: replace PacketParsing with something
	else that processes an int list. Lists are GC
	friendly, so should make life easier. Might also
	need to get rid of the Event channel; likely
	another bottleneck in the whole system...
*)

type packet = int list

exception Truncated

let i32 = function
	| a :: b :: c :: d :: rem ->
		Int32.logor
			(Int32.shift_left (Int32.of_int a) 24)
			(Int32.of_int ((b lsl 16) lor (c lsl 8) lor d)),
		rem
	| _ -> raise Truncated

let i16 = function
	| a :: b :: rem ->
		(a lsl 8) lor b,
		rem
	| _ -> raise Truncated

let i8 = function
	| a :: rem -> a, rem
	| _ -> raise Truncated

let eth_addr = function
	| a :: b :: c :: d :: e :: f :: rem ->
		NetworkProtocolStack.Ethernet.Addr (a,b,c,d,e,f),
		rem
	| _ -> raise Truncated

let ip_addr = function
	| a :: b :: c :: d :: rem ->
		NetworkProtocolStack.IPv4.Addr (a,b,c,d),
		rem
	| _ -> raise Truncated

let ipv4_addr = ip_addr

let rec drop n = function
	| list when n = 0 -> list
	| _ :: list -> drop (n-1) list
	| [] -> []

let rec from_ba ba offset count acc =
	if count = 0 then List.rev acc
	else begin
		let offset =
			if offset = Bigarray.Array1.dim ba then 0 else offset
		in
		from_ba ba (offset+1) (count-1) (ba.{offset} :: acc)
	end

module Ethernet = struct
	type packet = {
		dstAddr : NetworkProtocolStack.Ethernet.addr;
		srcAddr : NetworkProtocolStack.Ethernet.addr;
		protocol : int;
		content : int list;
	}
	
	let parse bytes =
		let dst, bytes = eth_addr bytes in
		let src, bytes = eth_addr bytes in
		let proto, bytes = i16 bytes in
		{ dstAddr = dst; srcAddr = src; protocol = proto; content = bytes }
end

module IPv4 = struct
	(* packet is 5 dwords + options (header length - 5) + (length - header length) dwords *)
	type packet = {
		tos : int;
		ttl : int;
		protocol : int;
		srcAddr : NetworkProtocolStack.IPv4.addr;
		dstAddr : NetworkProtocolStack.IPv4.addr;
		contentLength : int;
		content : int list;
	}
	
	let parse bytes =
		let hdr, bytes = i8 bytes in (* 0 *)
		let tos, bytes = i8 bytes in (* 1 *)
		let size, bytes = i16 bytes in (* 2 - 3 *)
		let ttl, bytes = i8 (drop 4 bytes) in (* 8 *)
		let proto, bytes = i8 bytes in (* 9 *)
		let src, bytes = ipv4_addr (drop 2 bytes) in (* 12 - 15 *)
		let dst, bytes = ipv4_addr bytes in (* 16 - 19 *)
		if hdr <> 0x45 then
			failwith "IPv4.parse: can't handle IP options yet";
		let len = size - 20 in
			(* we'd have failed by now if that isn't the correct length :P *)
		{ tos = tos; ttl = ttl; protocol = proto; srcAddr = src; dstAddr = dst;
		  contentLength = len; content = bytes }
end

module UDP = struct
	type packet = {
		src : int;
		dst : int;
		content : int list;
	}
	
	let parse bytes =
		let src, bytes = i16 bytes in
		let dst, bytes = i16 bytes in
		let content = drop 4 bytes in
		{ src = src; dst = dst; content = content }
end

module TCP = struct
	type packet = {
		src : int;
		dst : int;
		seq : int32;
		ack : int32;
		flags : int;
		window : int;
		(*contentLength : int;*)
		headerSize : int;
		content : int list;
	}
	
	let parse bytes =
		let src, bytes = i16 bytes in
		let dst, bytes = i16 bytes in
		let seq, bytes = i32 bytes in
		let ack, bytes = i32 bytes in
		let size, bytes = i8 bytes in
		let flags, bytes = i8 bytes in
		let win, bytes = i16 bytes in
		(* content_length = packet_length - 20 -- why is packet_length an arg? *)
		{ src = src; dst = dst; seq = seq; ack = ack; headerSize = 20;
		  flags = flags land 0x3F; window = win; content = drop 4 bytes }
end

module ARP = struct
	type packet = {
		(*hardware : int;
		protocol : int;*)
		opcode : int;
		senderEth : NetworkProtocolStack.Ethernet.addr;
		senderAddr : NetworkProtocolStack.IPv4.addr;
		targetEth : NetworkProtocolStack.Ethernet.addr;
		targetAddr : NetworkProtocolStack.IPv4.addr;
	}
	
	let parse bytes =
		let hardware, bytes = i16 bytes in
		let protocol, bytes = i16 bytes in
		let opcode, bytes = i16 (drop 2 bytes) in
		if hardware <> 1 || protocol <> 0x0800 then
			failwith "ARP.parse: only understand IPv4 over Ethernet";
		let sEth, bytes = eth_addr bytes in
		let sAddr, bytes = ipv4_addr bytes in
		let tEth, bytes = eth_addr bytes in
		let tAddr, bytes = ipv4_addr bytes in
		{ opcode = opcode; senderEth = sEth; senderAddr = sAddr;
		  targetEth = tEth; targetAddr = tAddr; }
end
