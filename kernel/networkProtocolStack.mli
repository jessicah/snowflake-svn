
(* The network protocol stack... *)

val checksum : Bitstring.t -> int
(** [checksum bitstring] calculates a word-sized checksum (16bits) *)

(* some generic parse/unparse functions *)

val parse_string : Bitstring.t -> string
val unparse_string : string -> Bitstring.t

val parse_array : Bitstring.t -> int array
val unparse_array : int array -> Bitstring.t

val parse_list : Bitstring.t -> int list
val unparse_list : int list -> Bitstring.t

module Ethernet : sig

	type addr = Addr of int * int * int * int * int * int
	
	type t = { dst : addr; src : addr; protocol : int; content : Bitstring.t }
	
	val addr_printer : unit -> addr -> string
	
	val parse_addr : Bitstring.t -> addr
	
	val parse : Bitstring.t -> t
	
	val unparse_addr : addr -> Bitstring.t
	
	val unparse : t -> Bitstring.t
	
	val broadcast : addr
	
	val invalid : addr
	
	val to_string : addr -> string

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
	
	val invalid : addr
	
	val to_string : addr -> string

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

module TCP : sig

	type flags = Urgent | Ack | Push | Reset | Syn | Finish
	
	type t = {
		src_port : int;
		dst_port : int;
		seq_num : int32;
		ack_num : int32;
		flags : flags list;
		window : int;
		content : Bitstring.t;
	}
	
	val parse : Bitstring.t -> t
	
	val unparse : t -> IPv4.addr -> IPv4.addr -> Bitstring.t

end
