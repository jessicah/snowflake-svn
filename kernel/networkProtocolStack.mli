
(* The network protocol stack... *)

val checksum : Bitstring.t -> int
(** [checksum bitstring] calculates a word-sized checksum (16bits) *)

module Ethernet : sig

	type addr = Addr of int * int * int * int * int * int
	
	type t = { dst : addr; src : addr; protocol : int; content : Bitstring.t }
	
	val addr_printer : unit -> addr -> string
	
	val parse_addr : Bitstring.t -> addr
	
	val parse : Bitstring.t -> t
	
	val unparse_addr : addr -> Bitstring.t
	
	val unparse : t -> Bitstring.t
	
	val broadcast : addr

end

module IPv4 : sig

	type addr = Addr of int * int * int * int
	
	type t = {
		tos : int;
		ttl : int;
		protocol : int;
		src : addr;
		dst : addr;
		options : Bitstring.t;
		content : Bitstring.t;
	}
	
	val addr_printer : unit -> addr -> string
	
	val parse_addr : Bitstring.t -> addr
	
	val parse : Bitstring.t -> t
	
	val unparse_addr : addr -> Bitstring.t
	
	val unparse : t -> Bitstring.t
	
	val broadcast : addr

end

module UDP : sig

	type t = {
		src: int;
		dst: int;
		content : Bitstring.t;
	}
	
	val parse : Bitstring.t -> t
	
	val unparse : t -> IPv4.addr -> IPv4.addr -> Bitstring.t

end
