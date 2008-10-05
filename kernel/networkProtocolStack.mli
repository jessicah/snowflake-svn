
(* The network protocol stack... *)

open IO
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
