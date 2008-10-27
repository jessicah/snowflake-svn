
open NetworkProtocolStack

type client = {
	send : string -> unit;
	recv : unit -> string;
	addr : Ethernet.addr;
	mutable ip : IPv4.addr;
}

val create : NetworkStack.net_device -> client

val register : client -> unit
