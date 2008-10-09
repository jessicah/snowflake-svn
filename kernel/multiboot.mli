
open Bigarray

type data = (char, int8_unsigned_elt, c_layout) Array1.t

external open_module : unit -> data = "caml_multiboot_module"
