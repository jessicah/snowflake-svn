
open Parser

module Eth = Ethernet
module IP = IPv4

type packet = {
		opcode : operation;
		sender : Eth.addr * IP.addr;
		target : Eth.addr * IP.addr;
	}
and operation
	= Request
	| Reply

let operation =
	let make = function
		| 1 -> Request
		| 2 -> Reply
		| _ -> raise Invalid_packet
	in make >>= word16

let packet =
	let parser =
		(fun op s1 s2 t1 t2 -> {
				opcode = op;
				senderHA = s1;
				senderIP = s2;
				targetHA = t1;
				targetIP = t2;
			})
		>>= operation >> Eth.addr >> IP.addr
		 >> Eth.addr >> IP.addr in
	(* hwtype = 0x0001; protocol = 0x0800 (ipv4) *)
	(* hLen = 0x06; pLen = 0x04 *)
	check16 0x0001 >>! check16 0x0800 >>!
	check8 0x06 >>! check8 0x04 >>! parser
