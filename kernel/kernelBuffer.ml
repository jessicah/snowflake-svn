
(* kernel buffer I/O *)

type sink = {
		buffer : unit -> BlockIO.t;
	}

type source = {
		mutable position : int;
		mutable length : int;
		mutable units : int;
		fill : BlockIO.t -> int -> int -> int;
	}

(* i'm still quite unsure about the above interfaces, but it's a start *)

let of_blockIO bio = fun () -> bio

let pos s = s.position
let len s = s.length
let units s = s.units
let fill sink source ofs len = source.fill (sink.buffer ()) ofs len
