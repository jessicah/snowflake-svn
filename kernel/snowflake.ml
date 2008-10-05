
let echo_shell () =
	while true do
		Vt100.printf "%c" (Keyboard.get_char ())
	done

let (++) x f = f x

let print_ip () = function IPv4.Addr (x1,x2,x3,x4) ->
	Printf.sprintf "%d.%d.%d.%d" x1 x2 x3 x4

let print_dhcp packet =
	(*Vt100.printf "Transaction ID: %ld\r\n" packet.DHCP.transaction;*)
	Vt100.printf "Client IP: %a\r\n" print_ip packet.DHCP.client;
	Vt100.printf "Server IP: %a\r\n" print_ip packet.DHCP.server;
	(*Vt100.printf "Options: %d present\r\n" (List.length packet.DHCP.options);
	begin try 
		let z = List.find (fun o -> (fst o) = 0x35) packet.DHCP.options in
		Vt100.printf "Message: %d\r\n" (List.hd (snd z))
	with Not_found -> () end*) ()

let print_device dev =
	Vt100.printf "PCI: %04X:%04X\r\n" dev.PCI.vendor dev.PCI.device

open PCI

let rec (<->) a = function
	| b when a < b -> a :: (<->) (a + 1) b
	| b when a > b -> List.rev (b <-> a)
	| b -> [b]

let (>>) f x = f x

module DHCPClient = struct
	
	module P = NetworkProtocolStack

	type client = {
		send : string -> unit;
		recv : unit -> string;
		addr : int list;
		mutable ip   : int list;
	}
	
	let parser =
		DHCP.packet ++ UDP.packet ++ IPv4.packet ++ Ethernet.packet
	
	let as_packet string =
		let array = Array.create (String.length string) 0 in
		for i = 0 to Array.length array - 1 do
			array.(i) <- Char.code string.[i];
		done;
		Packet.of_array array
	
	let create (r,s,a) = {
			send = s;
			recv = r;
			addr = a;
			ip   = [0; 0; 0; 0];
		}
	
	let make_packet client ptype =
		let pad n = List.map (fun _ -> 0) (1 <-> n) in
		let payload = 
			[1;1;6;0;0x11;0x22;0x33;0x44;0;0;0x80;0x00]
			@ pad 16
			@ client.addr
			@ pad 202
			@ [0x63;0x82;0x53;0x63]
			@ ptype
			@ [0x37;0x07;0x01;0x1c;0x02;0x03;0x0f;0x06;0x0c;0xff]
			@ (pad (300 - (248 + List.length ptype)))
		in
		P.Ethernet.compose client.addr P.Ethernet.broadcast 0x0800 begin
		P.IP.compose 0x04 0x00 0xFF 0x11 client.ip P.IP.broadcast begin
		P.UDP.compose (68, client.ip) (67, P.IP.broadcast) payload end end
	
	let register client =
		Vt100.printf "Sending DHCP Discover...\r\n";
		let p = make_packet client [0x35;0x01;0x01] in
		client.send (ExtString.String.implode (Obj.magic p : char list));
		
		Vt100.printf "Awaiting DHCP Reply...\r\n";
		let reply = Parser.parse_packet parser (as_packet (client.recv ())) in
		let ip = reply.Ethernet.content.IPv4.content.UDP.content.DHCP.client in
		
		(* Assuming we received reply [0x02] :P *)
		
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
		print_dhcp reply.Ethernet.content.IPv4.content.UDP.content
end

let () =
    Vga.init (); (* set up a pretty console font *)
	Keyboard.init (); (* set up the keyboard handler *)
	Vt100.printf "Hello, from ML :)\nUsing ocaml version: %s\n" Sys.ocaml_version;
	Asm.sti ();
	let pci_devices = PCI.probe_bus () in
	(*List.iter print_device pci_devices;*)
	begin try
		let dev = List.find (fun d -> d.vendor = 0x10EC && d.device = 0x8139) pci_devices in
		let x = RealTek8139.create dev in
		Vt100.printf "Created realtek 8139 device\r\n";
		let client = DHCPClient.create x in
		DHCPClient.register client;
	with Not_found ->
		Vt100.printf "No realtek 8139 found\r\n";
	end;
	ignore (Thread.create echo_shell ()) (* start the echo shell *)
