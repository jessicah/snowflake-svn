
open Packet

type input
	= ByteAligned of inpacket
	| Unaligned of int * inpacket

type 'a output = Out of 'a * input

type 'a parser = P of (input -> 'a output)

exception Invalid_packet
exception End_of_packet
exception Check_failure

val empty_in : input

val parse : 'a parser -> input -> 'a output

val parse_packet : 'a parser -> inpacket -> 'a

val succ' : inpacket -> inpacket
val succ  : input -> input

val (>>=) : ('a -> 'b) -> 'a parser -> 'b parser
val (>>)  : ('a -> 'b) parser -> 'a parser -> 'b parser
val (>>!) : bool parser -> 'b parser -> 'b parser
val remainder : int array parser
val truncate : int -> unit parser

val bind : 'a parser -> ('a -> 'b parser) -> 'b parser
val return : 'a -> 'a parser
val guard : bool -> unit parser

val onebit : bool parser
val word8 : int parser
val word16 : int parser
val word32 : int parser
val iword32 : Int32.t parser
val bytes : int -> int list parser
val bits : int -> int parser
val check8 : int -> bool parser
val check16 : int -> bool parser
