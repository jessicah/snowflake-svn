

(* ELF -- The Executable and Linking Format *)

type header

val parse_elf_header : Bitstring.t -> header

val print_header : header -> unit
