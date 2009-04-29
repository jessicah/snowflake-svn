
open Bigarray

external out8  : int -> int -> unit = "snowflake_out8" "noalloc"
external out16 : int -> int -> unit = "snowflake_out16" "noalloc"
external out32 : int -> int32 -> unit = "snowflake_out32" "noalloc"

external in8  : int -> int = "snowflake_in8" "noalloc"
external in16 : int -> int = "snowflake_in16" "noalloc"
external in32 : int -> int32 = "snowflake_in32"

external in16s : int -> int -> string = "snowflake_in16s"
external out16s : int -> string -> int -> unit = "snowflake_out16s"

external hlt : unit -> unit = "snowflake_hlt" "noalloc"

external cli : unit -> unit = "snowflake_cli" "noalloc"
external sti : unit -> unit = "snowflake_sti" "noalloc"

(* returns a pointer to the memory chunk of the bigarray *)
external address : ('a, 'b, c_layout) Array1.t -> int32 = "snowflake_address"

(* given a pointer, and a size, wraps it in a bigarray *)

external array8 : int32 -> int -> ('a, int8_unsigned_elt, c_layout) Array1.t
	= "snowflake_array8"
external array16 : int32 -> int -> (int, int16_unsigned_elt, c_layout) Array1.t
	= "snowflake_array16"
external array32 : int32 -> int -> (int32, int32_elt, c_layout) Array1.t
	= "snowflake_array32"

external matrix8 : int32 -> int -> int -> ('a, int8_unsigned_elt, c_layout) Array2.t
	= "snowflake_matrix8"
external matrix16 : int32 -> int -> int -> (int, int16_unsigned_elt, c_layout) Array2.t
	= "snowflake_matrix16"
external matrix32 : int32 -> int -> int -> (int32, int32_elt, c_layout) Array2.t
	= "snowflake_matrix32"

external peek32 : int32 -> int32 = "snowflake_peek32"
external poke32 : int32 -> int32 -> unit = "snowflake_poke32"
external peek32_offset : int32 -> int -> int32 = "snowflake_peek32_offset"
external poke32_offset : int32 -> int -> int32 -> unit = "snowflake_poke32_offset"
