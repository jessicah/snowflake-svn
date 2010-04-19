
val create : int -> (unit -> unit) -> unit
(** [create irq cb] sets up an interrupt handler for [irq],
    which executes [cb] in a separate thread when the interrupt
	is signalled. *)

val create_i : int -> (unit -> unit) -> unit
