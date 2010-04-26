
(* The Network Stack *)

(* Need to tidy this stuff up a bit *)

(* Should do two things:
   1. Create a block device (provides the read and write capabilities) with a name/path/something...
   2. Register itself with the network stack using the identifier above
*)

type rx_channel = int list Event.channel

type net_device = {
	send : string -> unit;
	recv : unit -> PacketLists.packet;
	hw_addr : NetworkProtocolStack.Ethernet.addr
}

let devices = ref []

let m = Mutex.create ()
let netstack_ready = Condition.create ()

let register_device dev = match !devices with
	| [] ->
		devices := [dev];
		(* the netstack can start running *)
		Mutex.lock m;
		Condition.signal netstack_ready;
		Mutex.unlock m;
	| _ -> failwith "netstack: already have a nic!"
	
(* really, we shouldn't expose these! *)

let nic () = match !devices with
	| [] ->
		failwith "netstack: no nic"
	| x :: _ -> x

let send data = match !devices with
	| [] -> 
		Vt100.printf "netstack: no nic to send on!";
		failwith "netstack: no nic to send on!"
	| x :: _ -> x.send data

let recv () = match !devices with
	| [] -> 
		Vt100.printf "netstack: no nic to recv from!";
		failwith "netstack: no nic to recv from!"
	| x :: _ -> x.recv ()

let get_hw_addr () = match !devices with
	| [] ->
		Vt100.printf "netstack: no nic present!";
		failwith "netstack: no nic present!"
	| x :: _ -> x.hw_addr

module P = NetworkProtocolStack
module L = PacketLists

type settings = {
	mutable ip : P.IPv4.addr;
	mutable dns : P.IPv4.addr;
	mutable netmask : P.IPv4.addr;
	mutable gateway : P.IPv4.addr;
}

let settings = {
		ip = P.IPv4.invalid;
		dns = P.IPv4.invalid;
		netmask = P.IPv4.invalid;
		gateway = P.IPv4.invalid;
	}

let send_eth dst protocol content =
	send (Bitstring.string_of_bitstring
		(P.Ethernet.make dst (get_hw_addr ()) protocol content))

module ARP = struct
	(* provides address resolution service to the rest of the network stack *)
	(* currently, it's just for IPv4 :P *)
	let table = Hashtbl.create 8
	
	let m = Mutex.create ()
	let cv = Condition.create ()
	
	let rec lookup ip =
		begin try
			Hashtbl.find table ip
		with Not_found ->
			(* need to send an ARP request, and hope we get a reply :P *)
			let content = BITSTRING {
					0x0001 : 16; (* type = ethernet *)
					0x0800 : 16; (* protocol = IPv4 *)
					6 : 8; (* ehternet address 6 octets *)
					4 : 8; (* ipv4 address 4 octets *)
					0x0001 : 16; (* opcode = request *)
					P.Ethernet.unparse_addr (get_hw_addr ()) : 48 : bitstring; (* sender mac addr *)
					P.IPv4.unparse_addr settings.ip : 32 : bitstring; (* sender ip addr *)
					P.Ethernet.unparse_addr P.Ethernet.invalid : 48 : bitstring; (* dest mac addr *)
					P.IPv4.unparse_addr ip : 32 : bitstring (* dest ip addr *)
				}
			in
			let entries = Hashtbl.length table in
			(* send the packet! *)
			send_eth P.Ethernet.invalid 0x0806 content;
			(* wait for ARP table to be updated *)
			Mutex.lock m;
			while entries = Hashtbl.length table do
				Condition.wait cv m;
			done;
			Mutex.unlock m;
			lookup ip
		end
	
	let rec lookup2 ip =
			(* need to send an ARP request, and hope we get a reply :P *)
			let content = BITSTRING {
					0x0001 : 16; (* type = ethernet *)
					0x0800 : 16; (* protocol = IPv4 *)
					6 : 8; (* ehternet address 6 octets *)
					4 : 8; (* ipv4 address 4 octets *)
					0x0001 : 16; (* opcode = request *)
					P.Ethernet.unparse_addr (get_hw_addr ()) : 48 : bitstring; (* sender mac addr *)
					P.IPv4.unparse_addr settings.ip : 32 : bitstring; (* sender ip addr *)
					P.Ethernet.unparse_addr P.Ethernet.invalid : 48 : bitstring; (* dest mac addr *)
					P.IPv4.unparse_addr ip : 32 : bitstring (* dest ip addr *)
				}
			in
			(* send the packet! *)
			send_eth P.Ethernet.invalid 0x0806 content
	
	let resolve ip =
		(* check netmask, and change ip to gateway if it doesn't match *)
		let P.IPv4.Addr(la,lb,lc,ld) = ip in
		let P.IPv4.Addr(ra,rb,rc,rd) = settings.netmask in
		let ip =
			if la land ra = la && lb land rb = lb
				&& lc land rc = lc && ld land rd = ld
			then ip
			else settings.gateway
		in
		lookup ip
	
	let process packet =
		begin try
			let pkt = PacketLists.ARP.parse packet in
			let self = get_hw_addr () in
			if pkt.L.ARP.opcode = 1 && pkt.L.ARP.targetEth = self then begin
				(* send reply to request for our address *)
				Vt100.printf "ARP.process: request for our address\n";
				send_eth pkt.L.ARP.senderEth 0x0806 (BITSTRING {
					1 : 16; 0x0800 : 16; 6 : 8; 4 : 8; 2 : 16;
					P.Ethernet.unparse_addr self : 48 : bitstring;
					P.IPv4.unparse_addr settings.ip : 32 : bitstring;
					P.Ethernet.unparse_addr pkt.L.ARP.senderEth : 48 : bitstring;
					P.IPv4.unparse_addr pkt.L.ARP.senderAddr : 32 : bitstring
				})
			end else begin
				(*Vt100.printf "ARP.process: opcode: %d, target: %a\n"
					pkt.L.ARP.opcode P.Ethernet.addr_printer pkt.L.ARP.targetEth*)
			end;
			(* update the table if we already have the IP, or it's a reply to us *)
			(*if sender_ip <> P.IPv4.invalid && (Hashtbl.mem table sender_ip || target_eth = self) then begin*)
			if pkt.L.ARP.opcode = 2 then begin
				Hashtbl.replace table pkt.L.ARP.senderAddr pkt.L.ARP.senderEth;
				(*Vt100.printf "added ARP mapping: %s has mac %s\n"
					(P.IPv4.to_string sender_ip) (P.Ethernet.to_string sender_eth);*)
			end;
			(* notify of updates to the ARP table *)
			Mutex.lock m;
			Condition.signal cv;
			Mutex.unlock m
		with _ ->
			() (*Vt100.printf "ARP.process: unknown ARP\n"*)
		end
end

let send_ip protocol dst content =
	send_eth (ARP.resolve dst) 0x0800
		(P.IPv4.make protocol settings.ip dst content)

let send_udp src_port dst_port dst_ip content =
	send_ip 17 (* UDP over IP *) dst_ip
		(P.UDP.make src_port dst_port settings.ip dst_ip content)

let send_tcp src_port dst_port seq ack flags window dst_ip content =
	send_ip 6 (* TCP over IP *) dst_ip
		(P.TCP.make src_port dst_port seq ack flags window settings.ip dst_ip content)
	
(* run the network stack *)

(* hmm: where does routing go? need to put that in somewhere soon... *)

let tcp_bindings = Hashtbl.create 7

let bind_tcp port f = Hashtbl.add tcp_bindings port f
let unbind_tcp port = Hashtbl.remove tcp_bindings port

module Shell = struct
	(* set up an ipconfig to configure the network *)
	open Arg
	
	let of_string s =
		try
			Scanf.sscanf s "%d.%d.%d.%d" (fun a b c d ->
				P.IPv4.Addr (a,b,c,d))
		with _ ->
			raise (Bad "Failure parsing IP address")
	
	let set_ip s = settings.ip <- of_string s
	let set_mask s = settings.netmask <- of_string s
	let set_gw s = settings.gateway <- of_string s
	
	let print_settings () =
		Vt100.printf "Network settings:\n  IP:      %s\n  Netmask: %s\n  Gateway: %s\n"
			(P.IPv4.to_string settings.ip)
			(P.IPv4.to_string settings.netmask)
			(P.IPv4.to_string settings.gateway)
	
	(* ipconfig -ip 130.123.131.217 -mask 255.255.255.128 -gw 130.123.131.129 *)
	let init () =
		Shell.add_command "ipconfig" print_settings [
			"-ip", String set_ip, " IP Address";
			"-mask", String set_mask, " Net Mask";
			"-gw", String set_gw, " Gateway";
		]
end

let init () =
	(*(* hard-code the settings for now :P *)
	settings.ip <- P.IPv4.Addr (130,123,131,217);
	settings.netmask <- P.IPv4.Addr (255,255,255,128);
	settings.gateway <- P.IPv4.Addr (130,123,131,129);*)
	Shell.init ();
	(* hardcode eth addr for 130.123.131.129 cause it won't reply :( *)
	Hashtbl.add ARP.table (P.IPv4.Addr(130,123,131,129)) (P.Ethernet.Addr(0x00,0x12,0xDA,0xF7,0x77,0xFF));
	
	let send_arps () =
		while true do
			try
			Thread.yield ();
			ARP.lookup2 settings.gateway;
			for i = 0 to 1000 do
				Thread.yield ()
			done
			with _ -> ()
		done
	in
	
	let read_thread () =
		(* this is a blocking call until data ready *)
		while true do
		begin try
			let eth = L.Ethernet.parse (recv ()) in
			match eth.L.Ethernet.protocol with
				| 0x0806 ->
					ARP.process eth.L.Ethernet.content
				| 0x0800 ->
					let ipv4 = L.IPv4.parse eth.L.Ethernet.content in
					begin match ipv4.L.IPv4.protocol with
						| 6 -> (* TCP/IP *)
							let packet_length = ipv4.L.IPv4.contentLength in
							let tcp = L.TCP.parse ipv4.L.IPv4.content in
							let port = tcp.L.TCP.dst in
							begin try
								let f = Hashtbl.find tcp_bindings port in
								f tcp packet_length
							with Not_found ->
								(*Vt100.printf "No handler for TCP port %d\n" port*)
								()
							end
						| (*n -> Vt100.printf "IPv4: unknown protocol %d\n" n*)
						_ -> ()
					end
				| _ -> ()(*Vt100.printf "Ethernet: unknown protocol %d\n" n*)
		with ex ->
			Vt100.printf "netstack read: %s\n" (Printexc.to_string ex)
		end;
		done
	in			
	let thread_fun () =
		Mutex.lock m;
		while !devices = [] do
			Condition.wait netstack_ready m;
		done;
		Mutex.unlock m;
		(* start the read thread *)
		ignore (Thread.create read_thread () "netstack_read");
		(* send out ARP packets lots; see what happens *)
		ignore (Thread.create send_arps() "send_arps");
	in
	ignore (Thread.create thread_fun () "netstack_init")

module Helpers = struct
	let ip_addr = function
		| [a;b;c;d] -> P.IPv4.Addr(a,b,c,d)
		| _ -> failwith "Invalid IP address"
end

module type ETHERNET = sig
		type t
		val init : unit -> t
		val isr : t -> rx_channel -> unit -> unit
		val send : t -> string -> unit
		val address : t -> NetworkProtocolStack.Ethernet.addr
	end

module EthernetDriver : functor (Driver : ETHERNET) -> sig
		val init : int -> Driver.t
		val read : unit -> PacketLists.packet
		val write: Driver.t -> string -> unit
		val address: Driver.t -> NetworkProtocolStack.Ethernet.addr
	end = functor (Driver : ETHERNET) -> struct
		let rx_buffer = Event.new_channel ()
		
		let init irq = 
			let t = Driver.init () in
			Interrupts.create_i irq (Driver.isr t rx_buffer);
			t
		let read () = Event.sync (Event.receive rx_buffer)
		let write t packet = Driver.send t packet
		let address t = Driver.address t
	end

module EthernetStack = struct
	let create init read write irq addr =
		let t = init irq in {
			send = write t;
			recv = read;
			hw_addr = addr t;
		}
end
