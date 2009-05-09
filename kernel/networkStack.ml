
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
	
	let tcp_bindings = Hashtbl.create 7
	
	let bind_tcp port f =
		Hashtbl.add tcp_bindings port f
	
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
				opcode : 16; (* opcode = reply *)
				sender_mac : 48 : bitstring;
				sender_ip : 32 : bitstring;
				target_mac : 48 : bitstring;
				_ : 32 : bitstring
			} ->
				let ip = P.IPv4.parse_addr sender_ip in
				let mac = P.Ethernet.parse_addr sender_mac in
				(* update the table *)
				if ip <> P.IPv4.invalid then
					Hashtbl.replace table ip mac;
				(* notify that we have a new entry in our ARP table *)				
				Mutex.lock m;
				Condition.signal cv;
				Mutex.unlock m;
				(*Vt100.printf "Added new mapping: %s => %s\n"
					(P.IPv4.to_string ip) (P.Ethernet.to_string mac);*)
				if opcode = 1 && P.Ethernet.parse_addr target_mac = (get_hw_addr ()) then begin (* it's a request *)
					Vt100.printf "Got an ARP request\n";
					let content = BITSTRING {
						1 : 16; 0x0800 : 16; 6 : 8; 4 : 8; 2 : 16;
						P.Ethernet.unparse_addr (get_hw_addr ()) : 48 : bitstring;
						P.IPv4.unparse_addr settings.ip : 32 : bitstring;
						sender_mac : 48 : bitstring;
						sender_ip : 32 : bitstring
					} in
					send (Bitstring.string_of_bitstring (make_eth_packet mac 0x0806 content));
				end
			| { _ : -1 : bitstring } -> ()
	end
	
	let open_udp src_port dst_port dst_ip =
		failwith "open_udp"
	
	type tcp_state = Listen | Syn_sent | Syn_recv | Established | Fin_wait1 | Fin_wait2
		| Close_wait | Closing | Last_ack | Time_wait | Closed
	
	type tcb = {
		mutable snd_una : int32; (* send unacknowledged *)
		mutable snd_nxt : int32; (* sent next *)
		mutable iss : int32; (* initial send sequence number *)
		mutable rcv_nxt : int32; (* receive next *)
		mutable irs : int32; (* initial receive sequence number *)
		mutable window_size : int;
		mutable state : tcp_state;
	}
	
	let next_available_port = ref 65535
	
	let get_next_available_port () =
		let port = !next_available_port in
		decr next_available_port;
		port
	
	let open_tcp ip port =
		let in_data = Event.new_channel () in
		let out_data = Event.new_channel () in
		let obuf = Buffer.create 512 in
		let ibuf = Buffer.create 512 in
		
		let tcb = {
			snd_nxt = 0l;
			snd_una = 0l;
			iss = 0l;
			irs = 0l;
			rcv_nxt = 0l;
			state = Syn_sent;
			window_size = 512;
		} in
		
		let src_port = get_next_available_port () in
		
		let send flags data =
			let content =
				make_ip_packet 6 (* tcp *)ip (P.TCP.unparse {
					P.TCP.src_port = src_port;
					P.TCP.dst_port = port;
					P.TCP.seq_num = tcb.snd_nxt;
					P.TCP.ack_num = tcb.rcv_nxt;
					P.TCP.flags = flags;
					P.TCP.window = tcb.window_size;
					P.TCP.content = data;
				} settings.ip ip)
			in
			send (Bitstring.string_of_bitstring (make_eth_packet (ARP.lookup ip) 0x0800 content))
		in
		
		let rec input buf ofs len = begin match Buffer.length ibuf with
			| 0 -> (* nothing in the buffer *)
				Buffer.add_string ibuf (Event.sync (Event.receive in_data));
				input buf ofs len
			| n when n > len ->
				let left = Buffer.sub ibuf 0 len in
				let rest = Buffer.sub ibuf len (n - len) in
				Buffer.clear ibuf;
				Buffer.add_string ibuf rest;
				String.blit left 0 buf ofs len;
				len
			| n ->
				String.blit (Buffer.contents ibuf) 0 buf ofs n;
				Buffer.clear ibuf;
				n
			end
		in
		
		let rec flush () = begin match Event.poll (Event.receive out_data) with
			| None ->
				begin match Buffer.length obuf with
				| 0 -> ()
				| blen when blen > tcb.window_size ->
					let left = Buffer.sub obuf 0 tcb.window_size in
					let rest = Buffer.sub obuf tcb.window_size (blen - tcb.window_size) in
					Buffer.clear obuf;
					Buffer.add_string obuf rest;
					send [ P.TCP.Ack ] (Bitstring.bitstring_of_string left);
					tcb.snd_nxt <- Int32.add (Int32.of_int tcb.window_size) tcb.snd_nxt;
					flush ()
				| blen ->
					send [ P.TCP.Ack ] (Bitstring.bitstring_of_string (Buffer.contents obuf));
					tcb.snd_nxt <- Int32.add (Int32.of_int blen) tcb.snd_nxt;
					Buffer.clear obuf
				end
			| Some data ->
				begin match Buffer.length obuf, String.length data with
				| 0, len when len > tcb.window_size ->
					let left = String.sub data 0 tcb.window_size in
					Buffer.add_substring obuf data tcb.window_size (len - tcb.window_size);
					send [ P.TCP.Ack ] (Bitstring.bitstring_of_string left);
					tcb.snd_nxt <- Int32.add (Int32.of_int tcb.window_size) tcb.snd_nxt;
					flush ()
				| 0, len ->
					send [ P.TCP.Ack ] (Bitstring.bitstring_of_string data);
					tcb.snd_nxt <- Int32.add (Int32.of_int len) tcb.snd_nxt
				| blen, _ ->
					Buffer.add_string obuf data;
					let len = min tcb.window_size blen in
					let left = Buffer.sub obuf 0 len in
					let rest = Buffer.sub obuf len (blen - len) in
					Buffer.clear obuf;
					Buffer.add_string obuf rest;
					send [ P.TCP.Ack ] (Bitstring.bitstring_of_string left);
					tcb.snd_nxt <- Int32.add (Int32.of_int len) tcb.snd_nxt;
					flush ()
				end
			end
		in
		
		let process t =
			let set f = List.mem f t.P.TCP.flags in
			match tcb.state with
				| Syn_sent when set P.TCP.Syn && set P.TCP.Ack && t.P.TCP.ack_num = Int32.add 1l tcb.iss ->
					tcb.state <- Established;
					
					tcb.irs <- t.P.TCP.seq_num;
					tcb.rcv_nxt <- Int32.add 1l tcb.irs;
					
					(* send ack *)
					send [ P.TCP.Ack ] Bitstring.empty_bitstring;
					
					(* send any waiting data *)
					flush ()
				| Established when not (set P.TCP.Syn) && set P.TCP.Ack && t.P.TCP.seq_num = tcb.rcv_nxt ->
					let len = Bitstring.bitstring_length t.P.TCP.content in
					tcb.snd_una <- Int32.add (Int32.of_int len) tcb.snd_una;
					tcb.rcv_nxt <- Int32.add (Int32.of_int len) tcb.rcv_nxt;
					
					(* send ack *)
					send [ P.TCP.Ack ] Bitstring.empty_bitstring;
					
					(* send data somewhere *)
					Event.sync (Event.send in_data (Bitstring.string_of_bitstring t.P.TCP.content))
				| _ -> () (* minimalist tcp stack :P *)
		in
					
		(* bind function to process received tcp packets *)
		bind_tcp src_port process;
		
		(* initialise connection *)
		send [ P.TCP.Syn ] Bitstring.empty_bitstring;
		tcb.snd_nxt <- Int32.add 1l tcb.iss;
		
		(* now need to return I/O channels *)
		IO.from_in_channel (object
				method input buf ofs len =
					input buf ofs len
				method close_in () = () (* this should close the TCP connection *)
			end),
		IO.from_out_channel (object
				method output buf ofs len =
					Event.sync (Event.send out_data (String.sub buf ofs len));
					if len >= tcb.window_size then flush ();
					len
				method flush () = flush ()
				method close_out () = flush () (* this should close the TCP connection *)
			end)
			
end

(* run the network stack *)

let init () =
	(* hard-code the settings for now :P *)
	API.settings.API.ip <- P.IPv4.Addr (130,123,131,217);
	API.settings.API.netmask <- P.IPv4.Addr (255,255,255,128);
	API.settings.API.gateway <- P.IPv4.Addr (130,123,131,129);
	let read_thread () =
		(* this is a blocking call until data ready *)
		while true do
		begin try
			let eth = P.Ethernet.parse (Bitstring.bitstring_of_string (recv ())) in
			match eth.P.Ethernet.protocol with
				| 0x0806 -> (* got an ARP packet *)
					API.ARP.process_arp eth.P.Ethernet.content
				| 0x0800 -> (* got an IP packet *)
					let ip = P.IPv4.parse eth.P.Ethernet.content in
					begin match ip.P.IPv4.protocol with
					| 6 (* tcp *) ->
						begin
							let tcp = P.TCP.parse ip.P.IPv4.content in
							try
								let f = Hashtbl.find API.tcp_bindings tcp.P.TCP.dst_port in
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
