
(* ALAC : Bit Buffer *)

(* we're not using pointers, so don't need the "end" pointer *)
type t = {
	buffer : string;
	mutable current : int;
	mutable bit_index : int;
	byte_size : int;
}

let get bits offset =
	Char.code bits.buffer.[bits.current + offset]

let empty = { buffer = ""; current = 0; bit_index = 0; byte_size = 0 }

let create buffer byte_size =
	{ buffer = buffer; current = 0; bit_index = 0; byte_size = byte_size }

let from_bitstring (buffer,ofs,len) =
	{ buffer = buffer;
		current = ofs / 8;
		bit_index = ofs land 7;
		byte_size = len / 8 }

let copy { buffer; current; bit_index; byte_size } =
	{ buffer; current; bit_index; byte_size }

let read bits num_bits =
	let result = ((get bits 0) lsl 16) lor ((get bits 1) lsl 8) lor ((get bits 2)) in
	let result = result lsl bits.bit_index in
	let result = result land 0x00FF_FFFF in
	
	bits.bit_index <- bits.bit_index + num_bits;
	
	let result = result lsr (24 - num_bits) in
	
	bits.current <- bits.current + (bits.bit_index lsr 3);
	bits.bit_index <- bits.bit_index land 7;

	result

let read_small bits num_bits =
	let result = ((get bits 0) lsl 8) lor ((get bits 1)) in
	let result = result lsl bits.bit_index in
	let result = result land 0xFFFF in

	bits.bit_index <- bits.bit_index + num_bits;

	let result = result lsr (16 - num_bits) in

	bits.current <- bits.current + (bits.bit_index lsr 3);
	bits.bit_index <- bits.bit_index land 7;

	result

let read_one bits =
	let result = ((get bits 0) lsr (7 - bits.bit_index)) land 1 in

	bits.bit_index <- bits.bit_index + 1;

	bits.current <- bits.current + (bits.bit_index lsr 3);
	bits.bit_index <- bits.bit_index land 7;

	result

let peek bits num_bits =
	(((((get bits 0) lsr 16) lor ((get bits 1) lsr 8) lor ((get bits 2))) lsr bits.bit_index) land 0x00FF_FFFF) lsr (24 - num_bits)

let peek_one bits =
	((get bits 0) lsr (7 - bits.bit_index)) land 1

let unpack_ber_size bits =
	let size = ref 0 in
	let tmp = ref 0x80 in
	(* the compare after step; this works because the initial value passes test *)
	while !tmp land 0x80 <> 0 do
		tmp := read_small bits 8;
		(* step *)
		size := (!size lsl 7) lor (!tmp land 0x7);
	done;

	!size

let position bits = bits.current * 8 + bits.bit_index

let advance bits num_bits =
	if num_bits <> 0 then begin
		bits.bit_index <- bits.bit_index + num_bits;
		bits.current <- bits.current + (bits.bit_index lsr 3);
		bits.bit_index <- bits.bit_index land 7;
	end

let min x y = if x < y then x else y

let rec write bits bit_values num_bits =
	let inv_bit_index = 8 - bits.bit_index in
	if num_bits > 0 then begin
		let cur_num = min inv_bit_index num_bits in
		let tmp = bit_values lsr (num_bits - cur_num) in
		let shift = inv_bit_index - cur_num in
		let mask = 0xFF lsr (8 - cur_num) in
		let mask = mask lsl shift in

		bits.buffer.[bits.current] <-
			Char.chr (((get bits 0) land (lnot mask)) lor ((tmp lsl shift) land mask));
		
		if inv_bit_index = cur_num then begin
			bits.current <- bits.current + 1;
			bits.bit_index <- 0;
		end;

		write bits bit_values (num_bits - cur_num);
	end

(* not sure if the above is correct; this one more closely resembles original *)
let write bits bit_values num_bits =
	let inv_bit_index = ref (8 - bits.bit_index) in
	let num_bits = ref num_bits in
	while !num_bits > 0 do
		let cur_num = min !inv_bit_index !num_bits in
		let tmp = bit_values lsr (!num_bits - cur_num) in
		let shift = !inv_bit_index - cur_num in
		let mask = 0xFF lsr (8 - cur_num) in
		let mask = mask lsl shift in
		
		bits.buffer.[bits.current] <-
			Char.chr (((get bits 0) land (lnot mask)) lor ((tmp lsl shift) land mask));

		num_bits := !num_bits - cur_num;
		inv_bit_index := !inv_bit_index - cur_num;

		if !inv_bit_index = 0 then begin
			inv_bit_index := 8;
			bits.current <- bits.current + 1;
		end;
	done;

	bits.bit_index <- 8 - !inv_bit_index

let byte_align bits add_zeroes =
	if bits.bit_index <> 0 then begin
		if add_zeroes then
			write bits 0 (8 - bits.bit_index)
		else
			advance bits (8 - bits.bit_index)
	end

let rewind bits = function
	| 0 -> ()
	| num_bits when bits.bit_index >= num_bits ->
		bits.bit_index <- bits.bit_index - num_bits
	| num_bits ->
		let num_bits = num_bits - bits.bit_index in
		bits.bit_index <- 0;

		let num_bytes = num_bits / 8 in
		let num_bits = num_bits mod 8 in

		bits.current <- bits.current - num_bytes;

		if num_bits > 0 then begin
			bits.bit_index <- 8 - num_bits;
			bits.current <- bits.current - 1;
		end;

		if bits.current < 0 then begin
			bits.current <- 0;
			bits.bit_index <- 0;
		end

let reset bits =
	bits.current <- 0;
	bits.bit_index <- 0
