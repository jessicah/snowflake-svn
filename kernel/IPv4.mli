
open Parser

type addr =
	Addr of int * int * int * int

(*val make : int -> int -> int -> int -> addr*)
val loopback : addr
val addr : addr parser

type 'a packet = {
		version : int;
		headerLen : int;
		tos : typeOfService;
		totalLen : int;
		identifier : int;
		flags : flags;
		fragOff : int;
		timeToLive : int;
		protocol : protocol;
		headerCheck : int;
		src : addr;
		dst : addr;
		options : int list;
		content : 'a;
	}
and typeOfService = {
		precedence : precedence;
		lowDelay : bool;
		highThrough : bool;
		highReal : bool;
	}
and precedence
	= Routine
	| Priority
	| Immediate
	| Flash
	| Flash_override
	| CRITIC_ECP
	| Internetwork_control
	| Network_control
and flags = {
		don'tFrag : bool;
		moreFrags : bool;
	}
and protocol
	= ICMP
	| TCP
	| UDP
	| Unknown of int

val packet : 'a parser -> 'a packet parser
