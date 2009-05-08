
(* BlockIO module *)

open Bigarray

type t = (int, int8_unsigned_elt, c_layout) Array1.t

type input = {
	mutable pos : int;
	data : t;
}

val make : t -> input

val block_read : input -> int -> t

val blit : input -> t -> unit

val make_io : input -> IO.input
