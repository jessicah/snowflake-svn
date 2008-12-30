
open ElfTypes

val parse : string -> Bitstring.t -> ElfTypes.t

val parse_symbol_table : Bitstring.t -> ElfTypes.symbol_table_entry array
