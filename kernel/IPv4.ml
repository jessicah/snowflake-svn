
open Parser

type addr =
	Addr of int * int * int * int

let make x1 x2 x3 x4 =
	Addr (x1, x2, x3, x4)

let loopback =
	make 127 0 0 1

let addr =
	make >>= word8 >> word8 >> word8 >> word8

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

let precedence =
	let make = function
		| 0 -> Routine
		| 1 -> Priority
		| 2 -> Immediate
		| 3 -> Flash
		| 4 -> Flash_override
		| 5 -> CRITIC_ECP
		| 6 -> Internetwork_control
		| 7 -> Network_control
		| _ -> assert false
	in make >>= bits 3

let typeOfService =
	(fun prec lowDelay highThrough highReal _ -> {
			precedence = prec;
			lowDelay = lowDelay;
			highThrough = highThrough;
			highReal = highReal;
		}) >>= precedence >> onebit >> onebit
		>> onebit >> bits 2

let flags =
	(fun _ d m -> {
			don'tFrag = d;
			moreFrags = m;
		}) >>= onebit >> onebit >> onebit

let protocols = [
		1, ICMP;
		6, TCP;
		17, UDP;
	]

let protocol =
	let make x = try
			List.assoc x protocols
		with Not_found -> Unknown x
	in make >>= word8

let packet content =
	let header = (fun v hl -> v, hl) >>= bits 4 >> bits 4 in
	let parser (v, hl) = (fun
			tos totalLen identifier
			fs fragOff ttl proto
			checksum src dst options
			rest -> {
					version = v;
					headerLen = hl;
					tos = tos;
					totalLen = totalLen;
					identifier = identifier;
					flags = fs;
					fragOff = fragOff;
					timeToLive = ttl;
					protocol = proto;
					headerCheck = checksum;
					src = src;
					dst = dst;
					options = options;
					content = rest;
				}) >>= typeOfService >> word16 >> word16
			>> flags >> bits 13 >> word8 >> protocol
			>> word16 >> addr >> addr
			>> bytes ((hl-5)*4) >> content
	in P begin fun input ->
		let Out (arg, input) = parse header input in
		parse (parser arg) input
	end
