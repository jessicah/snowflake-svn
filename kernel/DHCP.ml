
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

let message =
	let make = function
		| 0x05 -> Ack
		| x -> Other x
	in make >>= word8

let rec options = P begin fun input ->
		let Out (kind, input) = parse word8 input in
		if kind = 0xFF then
			Out ([], input)
		else begin
			let Out (len, input) = parse word8 input in
			let Out (data, input) = parse (bytes len) input in
			let Out (tail, input) = parse options input in
			Out ((kind, data) :: tail, input)
		end
	end

let rec skip_until predicate p = P begin fun input ->
		let Out (value, input) = parse p input in
		if predicate value then
			Out ((), input)
		else
			parse (skip_until predicate p) input
	end

let packet =
	(fun _ transaction _ client server _ options _ -> {
			(* should check first four bytes is 1 1 6 x *)
			(* bootp, hw type, hw addr len, hops *)
			transaction = transaction;
			client = client;
			server = server;
			options = options;
		}) >>= bytes 4 >> iword32
		>> bytes 8 >> IPv4.addr >> IPv4.addr
		(* how to skip up to the magic cookie? *)
		>> skip_until ((=) 0x63825363l) iword32
		>> options >> remainder
