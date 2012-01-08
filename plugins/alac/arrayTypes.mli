
(* ALAC : Array Types *)

open Bigarray

type uint8a = (int, int8_unsigned_elt, c_layout) Array1.t
type uint16a = (int, int16_unsigned_elt, c_layout) Array1.t
type int8a = (int, int8_signed_elt, c_layout) Array1.t
type int16a = (int, int16_signed_elt, c_layout) Array1.t
(* there is no signed vs unsigned here *)
type int32a = (int32, int32_elt, c_layout) Array1.t
type uint32a = (int32, int32_elt, c_layout) Array1.t
