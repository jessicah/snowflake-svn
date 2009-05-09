
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

let register_device dev = match !devices with
	| [] -> devices := [dev]
	| _ -> failwith "netstack: already have a nic!"

let send data = match !devices with
	| [] -> failwith "netstack: no nic to send on!"
	| x :: _ -> x.send data

let recv () = match !devices with
	| [] -> failwith "netstack: no nic to recv from!"
	| x :: _ -> x.recv ()

let get_hw_addr () = match !devices with
	| [] -> failwith "netstack: no nic present!"
	| x :: _ -> x.hw_addr
	
module API = struct
	module P = NetworkProtocolStack
	
	type settings = {
		mutable ip : P.IPv4.addr;
		mutable dns : P.IPv4.addr;
		mutable netmask : P.IPv4.addr;
		mutable gateway : P.IPv4.addr;
	}
	
	let settings = { ip = P.IPv4.invalid; dns = P.IPv4.invalid; netmask = P.IPv4.invalid; gateway = P.IPv4.invalid }
	
	let make_ip_packet x = failwith "make_ip_packet"
	let make_udp_packet x = failwith "make_udp_packet"
	let make_eth_packet x = failwith "make_eth_packet"
	
	let open_udp src_port dst_port dst_ip =
		failwith "open_udp"
		
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
