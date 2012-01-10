
open Bigarray
open BitBuffer
open Mp4_alt
open Decoder

open MusicPlayer
open BlockIO

type t = {
	mutable mdat : Mp4_alt.box;
	mutable cookie : Decoder.specific_config;
	mutable buffer : string;
	mutable bitbuffer : BitBuffer.t;
}

let t = {
	mdat = { kind = ""; offset = 0; size = 0 };
	cookie = !Decoder.config;
	buffer = "";
	bitbuffer = BitBuffer.create "" 0;
}

let openfile filename =
	begin try
		let ic = open_in_bin filename in
		let cookie, mdat = Mp4_alt.openfile ic in
		seek_in ic cookie.offset;
		let buffer = String.make cookie.size '\000' in
		really_input ic buffer 0 cookie.size;
		let cookie = Decoder.init (Bitstring.bitstring_of_string buffer) in
		Decoder.print_specific_config cookie;
		(* set up global config *)
		Decoder.config := cookie;
		(* allocate mix buffers *)
		Decoder.mix_buffer_U := Array1.create int32 c_layout (Int32.to_int cookie.frame_length * 4);
		Decoder.mix_buffer_V := Array1.create int32 c_layout (Int32.to_int cookie.frame_length * 4);
		(* allocate dynamic predictor *)
		Decoder.predictor := Array1.create int32 c_layout (Int32.to_int cookie.frame_length * 4);
		(* "shift off" buffer shares memory with predictor buffer *)
		Decoder.shift_buffer := BigarrayUtils.int32_to_uint16 !Decoder.predictor;
		
		(* stuff for music player *)

		(* set up buffers *)
		let decode_buffer = Array1.create int8_unsigned c_layout (4096 lsl 6) in
		let size = (Int32.to_int cookie.max_frame_bytes) + 1 in
		t.buffer <- String.make (Int32.to_int cookie.max_frame_bytes+1) '\000';
		t.bitbuffer <- BitBuffer.create t.buffer (Int32.to_int cookie.max_frame_bytes+1);
		t.mdat <- mdat;
		t.cookie <- cookie;
		let blockio = {
			pos = Array1.dim decode_buffer - 44; (* we're only going to write wave header *)
			data = decode_buffer;
		} in
		
		(* write WAVE header *)
		let x c = Char.code c in
		Array.iteri (fun ofs byte -> decode_buffer.{blockio.pos+ofs} <- byte)
			[|	x 'R';x 'I';x 'F';x 'F';
				0xff;0xff;0xff;0xff;
				x 'W';x 'A';x 'V';x 'E';
				x 'f';x 'm';x 't';x ' ';
				0x10; 0x00; 0x00; 0x00;
				0x01; 0x00; 0x02; 0x00;
				0x44; 0xAC; 0x00; 0x00;
				0x10; 0xB1; 0x02; 0x00;
				0x04; 0x00; 0x10; 0x00;
				x 'd';x 'a';x 't';x 'a';
				0xff; 0xff; 0xff; 0xff; |]; (* length, don't care *)

		seek_in ic mdat.offset;
		(* done opening *)
		ic, blockio
	with ex ->
		Printf.eprintf "alac: %s\n" (Printexc.to_string ex);
		raise ex;
	end

let decode ic blockio =
	let size = (Int32.to_int t.cookie.max_frame_bytes) + 1 in

	let frame_length = Int32.to_int t.cookie.frame_length in
	let chunk_size = frame_length * t.cookie.num_channels * t.cookie.bit_depth / 2 in

	let length = t.mdat.offset + t.mdat.size - 8 in

	let continue = ref true in
	let offset = ref 0 in
	let ok = ref false in

	let last = ref 0 in

while !continue do

	(* read initial buffer *)
	t.bitbuffer.current <- 0;
	really_input ic t.buffer 0 (min size (in_channel_length ic - pos_in ic));

	let decoded = Decoder.decode t.bitbuffer (Array1.sub blockio.data !offset chunk_size) frame_length t.cookie.num_channels in
	let ok', decoded = match decoded with `ok n -> true, n | `fail (n,ex) ->
		Printf.eprintf "error decoding: %s\n" (Printexc.to_string ex);
		for i = 0 to 16 do
			Printf.eprintf " %02x" (Char.code t.buffer.[t.bitbuffer.current+i]);
		done;
		Printf.eprintf "\n";
		Printf.eprintf "end position = %d\n" (pos_in ic);
		Printf.eprintf "length = %d\n" length;
		Printf.eprintf "previous position = %d\n" !last;
		Printf.eprintf "current = %d\n" t.bitbuffer.current;
		Printf.eprintf "decoded = %d\n" n;
		Printf.eprintf "size should've decoded = %d\n" (pos_in ic - !last);
		false, n
	in
	ok := ok';
	Printf.eprintf "+";
	(* refill *)
	seek_in ic (pos_in ic - size + t.bitbuffer.current);
	last := pos_in ic;
	offset := !offset + decoded;
	
	continue := (pos_in ic < length && ok') && ((Array1.dim blockio.data - !offset) >= chunk_size);
done;

	(* shift decoded bytes to end of blockio buffer -- limitation with blockio *)
	if !offset < Array1.dim blockio.data then begin
		Array1.blit (* src dst *)
			(Array1.sub blockio.data 0 !offset)
			(Array1.sub blockio.data (Array1.dim blockio.data - !offset) !offset);
	end;
	blockio.pos <- Array1.dim blockio.data - !offset;
	(* and finally return *)
	(pos_in ic < length && !ok)

let () =
	MusicPlayer.register_decoder { openfile = openfile; decode = decode }
