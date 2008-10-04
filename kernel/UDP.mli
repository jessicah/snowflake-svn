
open Parser

type 'a packet = {
		src : int;
		dst : int;
		len : int;
		checksum : int;
		content : 'a;
	}

val packet : 'a parser -> 'a packet parser
