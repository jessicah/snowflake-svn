
(* Network Stack *)

type rx_channel = PacketLists.packet Event.channel

type net_device = {
	send : string -> unit;
	recv : unit -> PacketLists.packet;
	hw_addr : NetworkProtocolStack.Ethernet.addr
}

val register_device : net_device -> unit

(* this is all kind of random being here... *)
val nic : unit -> net_device
val send : string -> unit
val recv : unit -> PacketLists.packet
val get_hw_addr : unit -> NetworkProtocolStack.Ethernet.addr

val send_eth : NetworkProtocolStack.Ethernet.addr -> int -> Bitstring.t -> unit
val send_ip : int -> NetworkProtocolStack.IPv4.addr -> Bitstring.t -> unit
val send_udp : int -> int -> NetworkProtocolStack.IPv4.addr -> Bitstring.t -> unit
val send_tcp : int -> int -> int32 -> int32 -> NetworkProtocolStack.TCP.flags list -> int -> NetworkProtocolStack.IPv4.addr -> Bitstring.t -> unit

module Shell : sig
	val of_string : string -> NetworkProtocolStack.IPv4.addr
end

module Helpers : sig
	val ip_addr : int list -> NetworkProtocolStack.IPv4.addr
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
		val read : unit -> PacketLists.packet
		val write: Driver.t -> string -> unit
		val address: Driver.t -> NetworkProtocolStack.Ethernet.addr
	end

module EthernetStack : sig
	val create : (int -> 'a) -> (unit -> PacketLists.packet) -> ('a -> string -> unit) -> int -> ('a -> NetworkProtocolStack.Ethernet.addr) -> net_device
end

val bind_tcp : int -> (PacketLists.TCP.packet -> int -> unit) -> unit
val unbind_tcp : int -> unit

val init : unit -> unit

type settings = {
	mutable ip : NetworkProtocolStack.IPv4.addr;
	mutable dns : NetworkProtocolStack.IPv4.addr;
	mutable netmask : NetworkProtocolStack.IPv4.addr;
	mutable gateway : NetworkProtocolStack.IPv4.addr;
}

val settings : settings
