

(* ELF -- The Executable and Linking Format *)

type elf

type t = Object of elf | Archive of (string * elf) list

val parse_elf_header : Bitstring.t -> elf

val print_header : elf -> unit

val parse : string -> Bitstring.t -> t

val print : t -> unit
