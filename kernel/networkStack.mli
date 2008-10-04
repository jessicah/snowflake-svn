
(* Network Stack *)

type rx_channel = string Event.channel

module type ETHERNET = sig
		type t
		val init : unit -> t
		val isr : t -> rx_channel -> unit -> unit
		val send : t -> string -> unit
		val address : t -> int list
	end;;

module EthernetDriver : functor (Driver : ETHERNET) -> sig
		val init : int -> Driver.t
		val read : unit -> string
		val write: Driver.t -> string -> unit
		val address: Driver.t -> int list
	end

module EthernetStack : sig
	val create : (int -> 'a) -> (unit -> string) -> ('a -> string -> unit) -> int -> ('a -> int list) -> ((unit -> string) * (string -> unit) * int list)
end
