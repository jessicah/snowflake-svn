
(* Direct Memory Access *)

open Bigarray

val allocate : unit -> (int, int8_unsigned_elt, c_layout) Array1.t

val start_transfer : int -> (int, int8_unsigned_elt, c_layout) Array1.t -> int -> unit

val stop_transfer : int -> unit
