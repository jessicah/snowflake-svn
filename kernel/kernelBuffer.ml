
(* kernel buffer I/O *)

type sink = {
		buffer : unit -> BlockIO.input;
	}

type source = {
		mutable position : int; (* position in units *)
		mutable length : int; (* length in bytes? *)
		units : int; (* size of a [contiguous] block *)
		mutable offset : int; (* byte offset within a unit *)
		fill : BlockIO.input -> int -> int -> int;
		(* perhaps I need a "seek" function too? *)
	}

(* i'm still quite unsure about the above interfaces, but it's a start *)

let of_blockIO bio = fun () -> bio

let pos s = s.position
let len s = s.length
let units s = s.units
let fill sink source ofs len = source.fill (sink.buffer ()) ofs len

let seek src num_bytes =
	let units = num_bytes / src.units in
	let rem = num_bytes mod src.units in
	src.position <- src.position + units;
	src.offset <- src.offset + rem;
	if src.offset < 0 then begin
		src.offset <- src.offset + src.units;
		src.position <- src.position - 1;
	end else if src.offset >= src.units then begin
		src.offset <- src.offset - src.units;
		src.position <- src.position + 1;
	end

let null_sink = { buffer = fun () -> failwith "null_sink" }
let null_source = { position = 0; length = 0; units = 0; offset = 0; fill = fun _ _ _ -> failwith "null_source" }
