
open Parser

type packet
	= RRQ of string * string
	| WRQ of string * string
	| Data of int * int array
	| Ack of int
	| Error of int * string

val packet : packet parser
