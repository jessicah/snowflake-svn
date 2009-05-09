
(* The Network Stack *)

(* Need to tidy this stuff up a bit *)

(* Should do two things:
   1. Create a block device (provides the read and write capabilities) with a name/path/something...
   2. Register itself with the network stack using the identifier above
*)

type rx_channel = string Event.channel

type net_device = {
	send : string -> unit;
	recv : unit -> string;
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
	| [] -> failwith "netstack: no nic to send on!"
	| x :: _ -> x.send data

let recv () = match !devices with
	| [] -> failwith "netstack: no nic to recv from!"
	| x :: _ -> x.recv ()

let get_hw_addr () = match !devices with
	| [] -> failwith "netstack: no nic present!"
	| x :: _ -> x.hw_addr

module P = NetworkProtocolStack

module API = struct
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
	
	(* move the make_xxx_packet functions outside of the module? *)
	let make_ip_packet ?(tos = 0) ?(ttl = 255) protocol ?src dst content =
		let src = match src with
			| None -> settings.ip
			| Some ip -> ip
		in
		P.IPv4.unparse {
			P.IPv4.tos = tos;
			P.IPv4.ttl = ttl;
			P.IPv4.protocol = protocol;
			P.IPv4.src = src;
			P.IPv4.dst = dst;
			P.IPv4.options = Bitstring.empty_bitstring;
			P.IPv4.content = content;
		}
	
	let make_udp_packet src_port dst_port ?src_ip dst_ip content =
		let src_ip = match src_ip with
			| None -> settings.ip
			| Some ip -> ip
		in
		P.UDP.unparse {
			P.UDP.src = src_port;
			P.UDP.dst = dst_port;
			P.UDP.content = content;
		} src_ip dst_ip
	
	let make_eth_packet dst ?src protocol content =
		let src = match src with
			| None -> get_hw_addr ()
			| Some addr -> addr
		in
		P.Ethernet.unparse {
			P.Ethernet.dst = dst;
			P.Ethernet.src = src;
			P.Ethernet.protocol = protocol;
			P.Ethernet.content = content;
		}
	
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
				send (Bitstring.string_of_bitstring (make_eth_packet P.Ethernet.invalid 0x0806 content));
				(* wait for ARP table to be updated *)
				Mutex.lock m;
				while entries = Hashtbl.length table do
					Condition.wait cv m;
				done;
				Mutex.unlock m;
				lookup ip
			end
		
		let process_arp bits =
			bitmatch bits with {
				0x0001 : 16;
				0x0800 : 16;
				6 : 8;
				4 : 8;
				0x0002 : 16; (* opcode = reply *)
				sender_mac : 48 : bitstring;
				sender_ip : 32 : bitstring;
				_ : 48 : bitstring;
				_ : 32 : bitstring
			} ->
				let ip = P.IPv4.parse_addr sender_ip in
				let mac = P.Ethernet.parse_addr sender_mac in
				(* update the table *)
				Hashtbl.replace table ip mac;
				(* notify that we have a new entry in our ARP table *)				
				Mutex.lock m;
				Condition.signal cv;
				Mutex.unlock m;
				Vt100.printf "Added new mapping: %s => %s\n"
					(P.IPv4.to_string ip) (P.Ethernet.to_string mac)
			| { _ : -1 : bitstring } -> (* ignore ARP requests *) ()
	end
	
	let open_udp src_port dst_port dst_ip =
		failwith "open_udp"
		
end

(* run the network stack *)

let init () =
	(* hard-code the settings for now :P *)
	API.settings.API.ip <- P.IPv4.Addr (130,123,131,217);
	API.settings.API.netmask <- P.IPv4.Addr (255,255,255,128);
	API.settings.API.gateway <- P.IPv4.Addr (130,123,131,129);
	let read_thread () =
		(* this is a blocking call until data ready *)
		begin try
			let eth = P.Ethernet.parse (Bitstring.bitstring_of_string (recv ())) in
			match eth.P.Ethernet.protocol with
				| 0x0806 -> (* got an ARP packet *)
					API.ARP.process_arp eth.P.Ethernet.content
				| _ -> (* discard packet *)
					()
		with ex -> Vt100.printf "netstack read: %s\n" (Printexc.to_string ex) end
	in			
	let thread_fun () =
		Mutex.lock m;
		while !devices = [] do
			Condition.wait netstack_ready m;
		done;
		Mutex.unlock m;
		(* start the read thread *)
		ignore (Thread.create read_thread ())
	in
	ignore (Thread.create thread_fun ());
	(* test ARP lookup *)
	ignore (API.ARP.lookup (P.IPv4.Addr (130,123,131,228)))

module type ETHERNET = sig
		type t
		val init : unit -> t
		val isr : t -> rx_channel -> unit -> unit
		val send : t -> string -> unit
		val address : t -> NetworkProtocolStack.Ethernet.addr
	end

module EthernetDriver : functor (Driver : ETHERNET) -> sig
		val init : int -> Driver.t
		val read : unit -> string
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
			hw_addr = addr t
		}
end
