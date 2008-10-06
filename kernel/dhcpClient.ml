
let (>>) f x = f x
let (++) x f = f x

let rec (<->) a = function
	| b when a < b -> a :: (<->) (a + 1) b
	| b when a > b -> List.rev (b <-> a)
	| b -> [b]

(*
let bs =
			E.unparse {
				E.dst = E.broadcast;
				E.src = E.Addr (a,b,c,d,e,f);
				E.protocol = 0x0800;
				E.content = I.unparse {
					I.tos = 0;
					I.ttl = 255;
					I.protocol = 0x11; (* udp *)
					I.src = I.Addr (0,0,0,0);
					I.dst = I.broadcast;
					I.options = Bitstring.empty_bitstring;
					I.content = Bitstring.bitstring_of_string "Hello, world!";
				}
			}
		in
*)

open NetworkProtocolStack
module E = Ethernet
module I = IPv4
module U = UDP

type client = {
	send : string -> unit;
	recv : unit -> string;
	addr : E.addr;
	mutable ip : I.addr;
}

type t = {
		transaction_id : int32;
		protocol : int;
		options : option list;
	}
and option = {
		kind : int;
		data : Bitstring.t;
	}

let standard_options = {
		kind = 0x37;
		data = Bitstring.bitstring_of_string "\x01\x1C\x02\x03\x0F\x06\x0C";
	} :: []
	
let create (r,s,[x1;x2;x3;x4;x5;x6]) = {
		send = s;
		recv = r;
		addr = E.Addr(x1, x2, x3, x4, x5, x6);
		ip   = I.Addr(0, 0, 0, 0)
	}

let magic_cookie = 0x63_82_53_63__l

let rec parse_options bits acc = bitmatch bits with
	| { kind : 8; length : 8; data : length*8 : bitstring; rest : -1 : bitstring }
		when kind <> 0xFF -> parse_options rest ({ kind = kind; data = data} :: acc)
	| { kind : 8 } when kind = 0xFF -> []

let parse bits =
	bitmatch bits with
	| { _ : 32; t_id : 32; _ : 16; proto : 16;
		_ : 256 : bitstring; addr : 48 : bitstring;
		_ : 202*8 : bitstring; cookie : 32 : check(cookie = magic_cookie);
		rest : -1 : bitstring }
	-> {
		transaction_id = t_id;
		protocol = proto;
		options = parse_options rest [];
	}

let unparse_option t =
	BITSTRING {
		t.kind : 8;
		Bitstring.bitstring_length t.data / 8 : 8;
		t.data : -1 : bitstring
	}

let unparse t addr =
	let options = Bitstring.concat (List.map unparse_option t.options) in
	let pad n = String.make n '\000' in
	BITSTRING {
		"\001\001\006\000" : 32 : string;
		t.transaction_id : 32;
		0 : 16;
		t.protocol : 16;
		pad 16 : 128 : string;
		E.unparse_addr addr : 48 : bitstring;
		pad 202 : 202*8 : string;
		magic_cookie : 32;
		options : -1 : bitstring;
		0xFF : 8; (* end of options *)
		pad (300 - (249 + (Bitstring.bitstring_length options / 8))) : -1 : string
	}

let register client =
	Vt100.printf "Sending DHCP Discover...\r\n";
	client.send begin Bitstring.string_of_bitstring begin
		E.unparse {
			E.dst = E.broadcast;
			E.src = client.addr;
			E.protocol = 0x0800; (* IP *)
			E.content = I.unparse {
				I.tos = 0; I.ttl = 255; I.protocol = 0x11; (* UDP *)
				I.src = client.ip;
				I.dst = I.broadcast;
				I.options = Bitstring.empty_bitstring;
				I.content = U.unparse {
					U.src = 68;
					U.dst = 67;
					U.content = unparse {
						transaction_id = 0xCAFEBABE_l;
						protocol = 0x8000; (* broadcast *)
						options = {
							kind = 0x35;
							data = Bitstring.bitstring_of_string "\x01";
						} :: standard_options;
					} client.addr;
				} client.ip I.broadcast;
			}
		} end
	end;
	
	Vt100.printf "Awaiting DHCP Reply...\r\n";
	let reply = Bitstring.bitstring_of_string (client.recv ()) in
	Vt100.printf "Got Reply...\r\n";

	let reply' = parse (U.parse (I.parse (E.parse reply).E.content).I.content).U.content in
	
	let get_option kind options = List.find (fun o -> o.kind = kind) options in
	
	(*(* Assuming we received reply [0x02] :P *)
	
	let IPv4.Addr (a,b,c,d) = ip in
	let ip' = [a;b;c;d] in
	let IPv4.Addr (a,b,c,d) = reply.Ethernet.content.IPv4.content.UDP.content.DHCP.server in
	let server' = [a;b;c;d] in
	
	Vt100.printf "Requesting DHCP Lease...\r\n";
	let p = make_packet client ([0x35;0x01;0x03] @ [0x36;4] @ server' @ [0x32;4] @ ip') in
	client.send (ExtString.String.implode (Obj.magic p : char list));
	let reply = Parser.parse_packet parser (as_packet (client.recv ())) in
	
	(* Assuming we received acknowledge [0x05] :P *)
	
	(* Got it! *)
	Vt100.printf "Received DHCP Lease!\r\n";
	(* option 3 = route, option 6 = dns, option 1 = subnet *)
	client.ip <- ip';
	print_dhcp reply.Ethernet.content.IPv4.content.UDP.content*)
()
