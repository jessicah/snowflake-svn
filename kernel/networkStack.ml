
(* The Network Stack *)

(* Need to tidy this stuff up a bit *)

(* Should do two things:
   1. Create a block device (provides the read and write capabilities) with a name/path/something...
   2. Register itself with the network stack using the identifier above
*)

type rx_channel = PacketParsing.t Event.channel

type net_device = {
	send : string -> unit;
	recv : unit -> PacketParsing.t;
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
module PP = PacketParsing

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
	
	let process packet =
		if PP.ARP.is_ipv4_over_ethernet packet then begin
			let self = get_hw_addr () in
			let target_eth = PP.to_eth_addr (PP.ARP.target_eth packet) in
			let sender_eth = PP.to_eth_addr (PP.ARP.sender_eth packet) in
			let sender_ip = PP.to_ipv4_addr (PP.ARP.sender_ip packet) in
			if PP.ARP.opcode packet = 1 && target_eth = self then begin
				(* send reply to request for our address *)
				Vt100.printf "Got an ARP request\n";
				send_eth sender_eth 0x0806 (BITSTRING {
					1 : 16; 0x0800 : 16; 6 : 8; 4 : 8; 2 : 16;
					P.Ethernet.unparse_addr self : 48 : bitstring;
					P.IPv4.unparse_addr settings.ip : 32 : bitstring;
					P.Ethernet.unparse_addr sender_eth : 48 : bitstring;
					P.IPv4.unparse_addr sender_ip : 32 : bitstring
				})
			end;
			(* update the table if we already have the IP, or it's a reply to us *)
			if sender_ip <> P.IPv4.invalid && (Hashtbl.mem table sender_ip || target_eth = self) then begin
				Hashtbl.replace table sender_ip sender_eth;
				(*Vt100.printf "added ARP mapping: %s has mac %s\n"
					(P.IPv4.to_string sender_ip) (P.Ethernet.to_string sender_eth);*)
			end;
			(* notify of updates to the ARP table *)
			Mutex.lock m;
			Condition.signal cv;
			Mutex.unlock m
		end else begin
			Vt100.printf "unknown ARP kind: %04x:%04x\n" (PP.ARP.hw_type packet) (PP.ARP.proto_type packet)
		end
end

let send_ip protocol dst content =
	send_eth (ARP.lookup dst) 0x0800
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

let init () =
	(* hard-code the settings for now :P *)
	settings.ip <- P.IPv4.Addr (130,123,131,217);
	settings.netmask <- P.IPv4.Addr (255,255,255,128);
	settings.gateway <- P.IPv4.Addr (130,123,131,129);
	
	let read_thread () =
		(* this is a blocking call until data ready *)
		while true do
		begin try
			let packet = recv () in
			match PP.Ethernet.protocol packet with
				| 0x0806 ->
					ARP.process (PP.Ethernet.content packet)
				| 0x0800 ->
					let packet = PP.Ethernet.content packet in
					begin match PP.IPv4.protocol packet with
						| 6 ->
							let packet_length = PP.IPv4.content_length packet in
							let packet = PP.IPv4.content packet in
							let port = PP.TCP.dst packet in
							begin try
								let f = Hashtbl.find tcp_bindings port in
								f packet packet_length
							with Not_found ->
								Vt100.printf "No handler for TCP port %d\n" port
							end
						| _ -> ()
					end
				| _ -> ()
		with ex ->
			Vt100.printf "netstack read: %s\n" (Printexc.to_string ex)
		end
		done
	in			
	let thread_fun () =
		Mutex.lock m;
		while !devices = [] do
			Condition.wait netstack_ready m;
		done;
		Mutex.unlock m;
		(* start the read thread *)
		ignore (Thread.create read_thread () "netstack_read")
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
		val read : unit -> PacketParsing.t
		val write: Driver.t -> string -> unit
		val address: Driver.t -> NetworkProtocolStack.Ethernet.addr
	end = functor (Driver : ETHERNET) -> struct
		let rx_buffer = Event.new_channel ()
		
		let init irq = 
			let t = Driver.init () in
			Interrupts.create irq (Driver.isr t rx_buffer);
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
