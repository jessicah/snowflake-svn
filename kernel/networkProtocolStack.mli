
(* The network protocol stack... *)

module Ethernet : sig

	type addr = Addr of int * int * int * int * int * int
	
	type t = { dst : addr; src : addr; protocol : int; content : Bitstring.t }
	
	val addr_printer : unit -> addr -> string
	
	val parse_addr : Bitstring.t -> addr
	
	val parse : string -> t
	
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
	
	val parse : string -> t
	
	val unparse_addr : addr -> Bitstring.t
	
	val unparse : t -> Bitstring.t
	
	val broadcast : addr

end

(*open IO
open IO.BigEndian

type token = Byte | Word | DWord | Bytes of int | Remainder

val (<->) : int -> int -> int list
val compose_gen : token list -> int list list -> int list
val decompose_gen : token list -> int list -> int list list

module IP : sig

	val compose : int -> int -> int -> int -> int list -> int list -> int list -> int list
	val decompose : int list -> int list list
	val broadcast : int list

end

module UDP : sig

	val compose : (int * int list) -> (int * int list) -> int list -> int list

end

module Ethernet : sig

	val compose : int list -> int list -> int -> int list -> int list
	val broadcast : int list

end
*)