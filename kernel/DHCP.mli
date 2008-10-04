
open Parser

type packet = {
		transaction : int32;
		client : IPv4.addr;
		server : IPv4.addr;
		options : option list;
	}
and message (* don't need at present *)
	= Discover
	| Offer
	| Request
	| Ack
	| NegAck
	| Other of int
and option = int * int list

val packet : packet parser
