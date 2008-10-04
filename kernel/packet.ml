
type inpacket = {
		buffer : int array;
		from : int;
		len : int;
	}

let byte p x =
	p.buffer.(p.from + x)

let to_array p =
	Array.sub p.buffer p.from (p.len)

let of_array a = {
		buffer = a;
		from = 0;
		len = Array.length a;
	}

(* Don't need to worry about outpackets yet *)

type outpacket = {
		outlen : int;
		chunks : chunk list;
	}
and chunk = int array

type outpacketS = outpacket -> outpacket
