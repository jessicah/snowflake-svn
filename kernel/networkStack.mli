
(* Network Stack *)

type rx_channel = string Event.channel

type net_device = {
	send : string -> unit;
	recv : unit -> string;
	hw_addr : NetworkProtocolStack.Ethernet.addr
}

val register_device : net_device -> unit

val send : string -> unit
val recv : unit -> string
val get_hw_addr : unit -> NetworkProtocolStack.Ethernet.addr

module API : sig
	val make_ip_packet : ?tos:int -> ?ttl:int -> int -> ?src:NetworkProtocolStack.IPv4.addr -> NetworkProtocolStack.IPv4.addr -> Bitstring.t -> Bitstring.t
	val make_udp_packet : int -> int -> ?src_ip:NetworkProtocolStack.IPv4.addr -> NetworkProtocolStack.IPv4.addr -> Bitstring.t -> Bitstring.t
	val make_eth_packet : NetworkProtocolStack.Ethernet.addr -> ?src:NetworkProtocolStack.Ethernet.addr -> int -> Bitstring.t -> Bitstring.t
end

module type ETHERNET = sig
		type t
		val init : unit -> t
		val isr : t -> rx_channel -> unit -> unit
		val send : t -> string -> unit
		val address : t -> NetworkProtocolStack.Ethernet.addr
	end;;

module EthernetDriver : functor (Driver : ETHERNET) -> sig
		val init : int -> Driver.t
		val read : unit -> string
		val write: Driver.t -> string -> unit
		val address: Driver.t -> NetworkProtocolStack.Ethernet.addr
	end

module EthernetStack : sig
	val create : (int -> 'a) -> (unit -> string) -> ('a -> string -> unit) -> int -> ('a -> NetworkProtocolStack.Ethernet.addr) -> net_device
end
