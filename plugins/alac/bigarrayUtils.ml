
(* BigarrayUtils : Conversion between different array types *)

open Bigarray

(* this creates a copy of the data =/ *)

let of_string string =
	let ba = Array1.create char c_layout (String.length string) in
	for i = 0 to String.length string - 1 do
		Array1.set ba i string.[i];
	done;
	ba

external change_flags : ('a, 'b, c_layout) Array1.t -> ('c, 'd) kind -> int -> ('c, 'd, c_layout) Array1.t = "caml_ba_change_flags"

let int32_from_string string : ArrayTypes.int32a =
	change_flags (of_string string) int32 (String.length string / 4)

let int32_to_uint16 (ba : ArrayTypes.int32a) : ArrayTypes.uint16a =
	change_flags ba int16_unsigned (Array1.dim ba * 2)

let uint8_to_int16 (ba : ArrayTypes.uint8a) : ArrayTypes.int16a =
	change_flags ba int16_signed (Array1.dim ba / 2)

let int32_to_uint8 (ba : ArrayTypes.int32a) : ArrayTypes.uint8a =
	change_flags ba int8_unsigned (Array1.dim ba * 4)

external from_string : (int, int8_unsigned_elt) kind -> c_layout layout -> string -> (int, int8_unsigned_elt, c_layout) Array1.t = "caml_ba_from_string"

let from_string s = from_string int8_unsigned c_layout s
