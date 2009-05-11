
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
	
	let process_arp bits =
		bitmatch bits with {
			0x0001 : 16;
			0x0800 : 16;
			6 : 8;
			4 : 8;
			opcode : 16;
			sender_mac : 48 : bitstring;
			sender_ip : 32 : bitstring;
			target_mac : 48 : bitstring;
			_ : 32 : bitstring
		} ->
			let ip = P.IPv4.parse_addr sender_ip in
			let mac = P.Ethernet.parse_addr sender_mac in
			let target_mac = P.Ethernet.parse_addr target_mac in
			let this_mac = get_hw_addr () in
			(* update the table if we already have the IP, or it matches us *)
			if ip <> P.IPv4.invalid && (Hashtbl.mem table ip || target_mac = this_mac) then
				Hashtbl.replace table ip mac;
			(* notify that we have a new entry in our ARP table *)				
			Mutex.lock m;
			Condition.signal cv;
			Mutex.unlock m;
			(*Vt100.printf "Added new mapping: %s => %s\n"
				(P.IPv4.to_string ip) (P.Ethernet.to_string mac);*)
			if opcode = 1 && target_mac = this_mac then begin
				(* send reply to request for our mac address *)
				Vt100.printf "Got an ARP request\n";
				let content = BITSTRING {
					1 : 16; 0x0800 : 16; 6 : 8; 4 : 8; 2 : 16;
					P.Ethernet.unparse_addr (get_hw_addr ()) : 48 : bitstring;
					P.IPv4.unparse_addr settings.ip : 32 : bitstring;
					sender_mac : 48 : bitstring;
					sender_ip : 32 : bitstring
				} in
				send_eth mac 0x0806 content
			end
		| { _ : -1 : bitstring } -> ()

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
			let eth = P.Ethernet.parse (Bitstring.bitstring_of_string (recv ())) in
			match eth.P.Ethernet.protocol with
				| 0x0806 -> (* got an ARP packet *)
					ARP.process_arp eth.P.Ethernet.content
				| 0x0800 -> (* got an IP packet *)
					let ip = P.IPv4.parse eth.P.Ethernet.content in
					begin match ip.P.IPv4.protocol with
					| 6 (* tcp *) ->
						begin
							let tcp = P.TCP.parse ip.P.IPv4.content in
							try
								let f = Hashtbl.find tcp_bindings tcp.P.TCP.dst_port in
								f tcp
							with Not_found -> Vt100.printf "No handler for port %d...\n" tcp.P.TCP.dst_port
						end
					| _ -> ()
					end
				| _ -> (* discard packet *)
					()
		with ex -> Vt100.printf "netstack read: %s\n" (Printexc.to_string ex) end
		done
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
	ignore (Thread.create thread_fun ())

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
