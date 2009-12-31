
(* BlockIO module *)

(* ExtLib's IO module is okay, I'm just struggling to figure out how to
   get efficient block IO working with it.
   
   Basically, I have a wave file in memory, using a Bigarray. I'm reading
   it like a character stream, and ExtLib uses a string proxy to buffer the
   characters into. Then I use that stream, pull out 16-bit integer quantities,
   and shove them back into another bigarray.
   
   You know what's worse is that the wave data in memory could be blitted
   directly to the DMA buffer for the audio driver.
   
   So there's a whole bunch of inefficient reading, converting to chars stored
   in a string, and pulling them back out and turning them into 16-bit integers,
   and shoving it inefficiently into the driver.
   
   There has to be a better way! *)

open Bigarray

type t = (int, int8_unsigned_elt, c_layout) Array1.t (* perhaps type could be char? *)

type input = {
	mutable pos : int;
	data : t;
}

(* There are no output types at present, just input *)

let make data = {
		pos = 0;
		data = data;
	}

let block_read input size =
	if input.pos >= Array1.dim input.data then
		failwith "block_read: no more data";
	let size = min size (Array1.dim input.data - input.pos) in
	let return = Array1.sub input.data input.pos size in
	input.pos <- input.pos + size;
	return

(* Tries to copy directly from the input to the output *)
let blit input output =
	let size = Array1.dim output in
	if Array1.dim input.data - input.pos  < size then
		failwith "blit: not enough input";
	let pos = input.pos in
	input.pos <- input.pos + size;
	Array1.blit
		(Array1.sub input.data pos size)
		output

let blit_from_string str input =
	let size = String.length str in
	if Array1.dim input.data - input.pos < size then
		failwith "blit_from_string: string too big";
	let pos = input.pos in
	input.pos <- input.pos + size;
	Array1.blit_from_string
		str
		(Array1.sub input.data pos size)

let make_io input = IO.from_in_chars (object
		method get () =
			input.pos <- input.pos + 1;
			if input.pos = Array1.dim input.data then
				raise IO.No_more_input;
			char_of_int input.data.{input.pos - 1}
		method close_in () = ()
	end)
