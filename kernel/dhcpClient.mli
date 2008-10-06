
open NetworkProtocolStack

type client = {
	send : string -> unit;
	recv : unit -> string;
	addr : Ethernet.addr;
	mutable ip : IPv4.addr;
}

val create : (unit -> string) * (string -> unit) * int list -> client

val register : client -> unit
