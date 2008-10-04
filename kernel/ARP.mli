
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

val packet : packet parser
