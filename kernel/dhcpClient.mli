
open NetworkProtocolStack

type client = {
	send : string -> unit;
	recv : unit -> string;
	addr : Ethernet.addr;
	mutable ip : IPv4.addr;
}

exception Error of string

val create : NetworkStack.net_device -> client

val register : client -> IPv4.addr

val init : unit -> unit

(*
		QEMU VLAN      <------>  Firewall/DHCP server <-----> Internet
                           |          (10.0.2.2)
                           |
                           ---->  DNS server (10.0.2.3)
                           |
                           ---->  SMB server (10.0.2.4)
*)
