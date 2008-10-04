
type inpacket = {
		buffer : int array;
		from : int;
		len : int;
	}

val byte : inpacket -> int -> int
val to_array : inpacket -> int array
val of_array : int array -> inpacket
