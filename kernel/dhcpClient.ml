
let (>>) f x = f x
let (++) x f = f x

let rec (<->) a = function
	| b when a < b -> a :: (<->) (a + 1) b
	| b when a > b -> List.rev (b <-> a)
	| b -> [b]

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
		client_addr : I.addr;
		my_addr : I.addr;
		next_server : I.addr;
		relay_agent : I.addr;
		flags : int;
		options : option list;
	}
and option = {
		kind : int;
		data : int list;
	}

let standard_options = {
		kind = 0x37;
		data = [ 0x01; 0x02; 0x03; 0x06; 0x0C; 0x0F; 0x1C ]
	} :: []
	
let create d = {
		send = d.NetworkStack.send;
		recv = d.NetworkStack.recv;
		addr = d.NetworkStack.hw_addr;
		ip   = I.Addr(0, 0, 0, 0)
	}

let magic_cookie = 0x63_82_53_63__l

let rec parse_options bits acc = bitmatch bits with
	| { kind : 8; length : 8; data : length*8 : bitstring; rest : -1 : bitstring }
		when kind <> 0xFF ->
		parse_options rest ({ kind = kind; data = parse_list data} :: acc)
	| { kind : 8 } when kind = 0xFF -> acc

let parse bits =
	bitmatch bits with
	| { _ : 32; t_id : 32; _ (* econds elapsed *) : 16; flags : 16;
		(* client ip : my ip : next server ip : relay agent ip *)
		client_addr : 32 : bitstring;
		my_addr : 32 : bitstring;
		next_server : 32 : bitstring;
		relay_agent : 32 : bitstring;
		mac_addr : 48 : bitstring;
		_ (* server host name, boot file name *) : 202*8 : bitstring;
		cookie : 32 : check(cookie = magic_cookie);
		rest : -1 : bitstring }
	-> {
		transaction_id = t_id;
		client_addr = I.parse_addr client_addr;
		my_addr = I.parse_addr my_addr;
		next_server = I.parse_addr next_server;
		relay_agent = I.parse_addr relay_agent;
		flags = flags;
		options = parse_options rest [];
	}

let unparse_option t =
	BITSTRING {
		t.kind : 8;
		List.length t.data : 8;
		unparse_list t.data : -1 : bitstring
	}

let unparse t addr =
	let options = Bitstring.concat (List.map unparse_option t.options) in
	let pad n = String.make n '\000' in
	BITSTRING {
		"\001\001\006\000" : 32 : string;
		t.transaction_id : 32;
		0 : 16;
		t.flags : 16;
		I.unparse_addr t.client_addr : 32 : bitstring;
		I.unparse_addr t.my_addr : 32 : bitstring;
		I.unparse_addr t.next_server : 32 : bitstring;
		I.unparse_addr t.relay_agent : 32 : bitstring;
		E.unparse_addr addr : 48 : bitstring;
		pad 202 : 202*8 : string;
		magic_cookie : 32;
		options : -1 : bitstring;
		0xFF : 8; (* end of options *)
		pad (300 - (239 + (Bitstring.bitstring_length options / 8))) : -1 : string
	}

let send client options =
	client.send begin parse_string begin
		E.unparse {
			E.dst = E.broadcast; E.src = client.addr;
			E.protocol = 0x0800; (* IP *) E.content = I.unparse {
				I.tos = 0; I.ttl = 255; I.protocol = 0x11; (* UDP *)
				I.src = client.ip; I.dst = I.broadcast;
				I.options = Bitstring.empty_bitstring;
				I.content = U.unparse {
					U.src = 68;
					U.dst = 67;
					U.content = unparse {
						transaction_id = 0xCAFEBABE_l;
						flags = 0x8000; (* broadcast *)
						client_addr = I.Addr(0,0,0,0);
						my_addr = client.ip;
						next_server = I.Addr(0,0,0,0);
						relay_agent = I.Addr(0,0,0,0);
						options = options;
					} client.addr;
				} client.ip I.broadcast;
			}
		}
		end
	end

let recv client =
	parse (U.parse (I.parse (E.parse (unparse_string (client.recv()))).E.content).I.content).U.content

let find_option kind packet =
	List.find (fun opt -> opt.kind = kind) packet.options

let register client =
	Vt100.printf "Sending DHCP Discover...\r\n";
	send client ({ kind = 0x35; data = [ 0x01 ] } :: standard_options);
	
	Vt100.printf "Awaiting DHCP Offer...\r\n";
	let reply = recv client in
	
	begin try
		begin match (find_option 0x35 reply).data with
		| [0x02] ->
			(* Send DHCP Request *)
			let server = (find_option 0x36 reply).data in
			let I.Addr(a,b,c,d) = reply.my_addr in
			Vt100.printf "Sending DHCP Request...\r\n";
			send client (
				{ kind = 0x35; data = [ 0x03 ] } :: (* request *)
				{ kind = 0x36; data = server } :: (* dhcp server *)
				{ kind = 0x32; data = [a; b; c; d] } :: (* ip to request *)
				standard_options
			);
			Vt100.printf "Awaiting DHCP Acknowledge...\r\n";
			let reply = recv client in
			begin match (find_option 0x35 reply).data with
			| [0x05] ->
				(* Apply the new IP settings *)
				client.ip <- reply.my_addr;
				Vt100.printf "Applied IP settings from DHCP server\r\n";
				Vt100.printf "Client IP: %a\n" I.addr_printer client.ip
			| _ ->
				(* A response we don't know what to do with *)
				Vt100.printf "Error: expected DHCP acknowledge\n"
			end
		| _ ->
			(* A response we don't know what to do with *)
			Vt100.printf "Error: expected DHCP offer\n"
		end
	with Not_found ->
		(* server sent an option we don't understand *)
		Vt100.printf "Error: received invalid DHCP response\n"
	end
	
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
