
(* A Ring Buffer *)

(*
	This is to replace using a string Queue.t, as it allocates
	too much memory, and is probably causing problems with the
	GC getting invoked too much -- it's hard to tell really.
	
	It should be kind of like the Buffer module, but in the
	short term, we'll use a fixed buffer size (say 4MB), and
	return an error if we can't add more data, or possibly
	block.
	
	The idea is to use String.blit to copy a chunk of data
	into a (hopefully) larger buffer, and use wrap-around.
*)

type t = {
	mutable read_pos: int;
	mutable write_pos: int;
	mutable length: int;
	size: int;
	buffer: string;
	m: Mutex.t;
	cv: Condition.t;
	mutable closed: bool;
}

let create ?(buf_size = 0x40_0000) () =
	{
		read_pos = 0;
		write_pos = 0;
		length = 0;
		size = buf_size;
		buffer = String.make buf_size '\000';
		m = Mutex.create ();
		cv = Condition.create ();
		closed = false;
	}

(* simply write entire s to t, or fail if not enough room *)
let write t s =
	let l = String.length s in
	if (t.size - t.length) < l then
		failwith "ring buffer: not enough space";
	Mutex.lock t.m;
	if t.write_pos + l >= t.size then begin
		(* requires two blits *)
		let sz = t.size - t.write_pos in
		String.unsafe_blit
			s 0 (* src + offset *)
			t.buffer t.write_pos (* dst + offset *)
			sz; (* length *)
		String.unsafe_blit
			s sz (* src + offset *)
			t.buffer 0 (* wrap-around *)
			(l - sz); (* remaining length *)
		(* update write_pos & length *)
		t.write_pos <- (l - sz);
	end else begin
		(* done in a single blit *)
		String.unsafe_blit
			s 0 t.buffer t.write_pos l;
		(* update write_pos & length *)
		t.write_pos <- t.write_pos + l;
	end;
	t.length <- t.length + l;
	Condition.signal t.cv;
	Mutex.unlock t.m

let close t =
	t.closed <- true;
	Mutex.lock t.m;
	Condition.signal t.cv;
	Mutex.unlock t.m

(* read up to n bytes from the buffer *)
let read t buf ofs len =
	let l = min len t.length in
	Mutex.lock t.m;
	if t.read_pos + l >= t.size then begin
		(* requires two blits *)
		let sz = t.size - t.read_pos in
		String.unsafe_blit
			t.buffer t.read_pos
			buf ofs
			sz;
		String.unsafe_blit
			t.buffer 0
			buf (ofs + sz)
			(l - sz);
		(* update read_pos *)
		t.read_pos <- (l - sz);
	end else begin
		(* done in a single blit *)
		String.unsafe_blit
			t.buffer t.read_pos
			buf ofs l;
		(* update read_pos *)
		t.read_pos <- t.read_pos + l;
	end;
	t.length <- t.length - l;
	Mutex.unlock t.m;
	l (* return the bytes actually read *)

(* creates an IO.input channel for reading *)
let mk_input t =
	IO.from_in_channel(object
			method input buf ofs len =
				let r = read t buf ofs len in
				if r = 0 && t.closed = false then begin
					Mutex.lock t.m;
					Condition.wait t.cv t.m;
					Mutex.unlock t.m;
					read t buf ofs len
				end else r
			method close_in () = ()
		end)
