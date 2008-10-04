
open Parser

type addr =
	Addr of int * int * int * int * int * int

val broadcast : addr

val addr : addr parser

type 'a packet = {
		dst : addr;
		src : addr;
		packType : packetType;
		content : 'a;
	}
and packetType
	= Ethernet of int
	| IPv4
	| IPv6
	| ARP
	| Unknown of int

val packet : 'a parser -> 'a packet parser
