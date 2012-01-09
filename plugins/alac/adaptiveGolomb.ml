
(* ALAC: Adaptive Golomb Decoding *)

open ArrayTypes
open Int32

exception ALAC_parameter_error

type params = {
  mutable mb : int32;
  mutable mb0 : int32;
  mutable pb : int32;
  mutable kb : int32;
  mutable wb : int32;
  mutable qb : int32;
  mutable fw : int32;
  mutable sw : int32;
  mutable maxrun : int32;
}

let make_params ~mb:mb ~pb:pb ~kb:kb ~fw:fw ~sw:sw ~maxrun:maxrun =
  let wb = pred (shift_left one (kb))
  and qb = sub (shift_left one 9) (of_int pb)
  in {
    mb = of_int mb;
    mb0 = of_int mb;
    pb = of_int pb;
    kb = of_int kb;
    wb = wb;
    qb = qb;
    fw = of_int fw;
    sw = of_int sw;
    maxrun = of_int maxrun
  }

let std_params ~fw:fw ~sw:sw =
  let mb = 10
  and pb = 40
  and kb = 14
  and maxrun = 255
  in make_params ~mb:mb ~pb:pb ~kb:kb ~fw:fw ~sw:sw ~maxrun:maxrun

let lead m =
  let rec loop c j =
    if logand c m <> zero || j = 32
    then j
    else loop (shift_right_logical c 1) (j+1)in
  loop (shift_left one 31) 0

let lg3a x =
  31 - lead (add x (of_int 3))

let get_next_fromlong inlong suff =
  shift_right_logical inlong (32 - suff)

let peek_32 bits =
	let read_k i s k = logor (shift_left (of_int (Char.code bits.BitBuffer.buffer.[bits.BitBuffer.current+i])) s) k in
	read_k 0 24 (read_k 1 16 (read_k 2 8 (read_k 3 0 zero)))

let peek_big bits numbits =
	let load1 = peek_32 bits in
	let result = if (numbits + bits.BitBuffer.bit_index) > 32 then begin
		let result = shift_left load1 bits.BitBuffer.bit_index in
		let load2 = of_int (Char.code bits.BitBuffer.buffer.[bits.BitBuffer.current+4]) in
		let load2shift = 8 - (numbits+bits.BitBuffer.bit_index-32) in
		let load2 = shift_right_logical load2 load2shift in
		let result = shift_right_logical result (32-numbits) in
		logor result load2
	end else begin
		shift_right_logical load1 (32-numbits-bits.BitBuffer.bit_index)
	end in
	if numbits <> 32 then
		logand result (lognot (shift_left 0xffff_ffffl numbits))
	else result

let read_big bits numbits =
	let r = peek_big bits numbits in
	BitBuffer.advance bits numbits;
	r

(* k is used as a shift amount so has to be small, hence not an int32 *)
let dyn_get bits (m : int32) (k : int) =
	let max_prefix_16 = 9 in
	let max_datatype_bits_16 = 16 in

	let bit_offset = bits.BitBuffer.bit_index in
	let stream = shift_left (peek_big bits (32-bit_offset)) bit_offset in
	let prefix = lead (lognot stream) in

	if prefix >= max_prefix_16 then begin
		BitBuffer.advance bits (max_prefix_16 + max_datatype_bits_16);
		let stream = shift_left stream max_prefix_16 in
		shift_right_logical stream (32 - max_datatype_bits_16)
	end else begin
		BitBuffer.advance bits (prefix + k);

		let stream = shift_left stream (prefix + 1) in
		let v = shift_right_logical stream (32-k) in
		let result = mul (of_int prefix) m in
		if v < 2l then
			result
		else begin
			BitBuffer.advance bits 1;
			add result (sub v one)
		end
	end

let dyn_get_32bit bits (m : int32) (k : int) (maxbits : int) =
	let max_prefix_32 = 9 in

	let bit_offset = bits.BitBuffer.bit_index in
	let stream = shift_left (peek_big bits (32-bit_offset)) bit_offset in
	let result = lead (lognot stream) in
	
	if result >= max_prefix_32 then begin
		BitBuffer.advance bits max_prefix_32;
		read_big bits maxbits;
	end else begin
		BitBuffer.advance bits (result + 1);

		if k <> 1 then begin
			let stream = shift_left stream ( result + 1) in
			let result = mul (of_int result) m in
			let v = shift_right_logical stream (32-k) in
			BitBuffer.advance bits (k-1);
			if v > one then begin
				let result = add result (sub v one) in
				BitBuffer.advance bits 1;
				result
			end else begin
				result
			end
		end else of_int result
	end

let qbshift = 9
let qb = shift_left one qbshift
let mmulshift = 2
let mdenshift = qbshift - mmulshift - 1
let moff = 1 lsl (mdenshift - 2)
let bitoff = 24
let n_max_mean_clamp = 0xffffl
let n_mean_clamp_val = 0xffffl

(* out_num_bits is never used, max_size <= 16 *)
let dyn_decomp params bitstream (pc : int32a) num_samples max_size =
	let c = ref 0 in
	let mb = ref params.mb0 in
	let zmode = ref zero in
	let pb_local = params.pb in
	let kb_local = to_int params.kb in
	let wb_local = params.wb in
	let out = ref 0 in

	while !c < num_samples do
		let m = shift_right_logical !mb qbshift in
		let k = lg3a m in
		let k = if k < kb_local then k else kb_local in
		let m = sub (shift_left one k) one in
		
		let n = dyn_get_32bit bitstream m k max_size in

		let ndecode = add n !zmode in
		let multiplier = neg (logand ndecode one) in
		let multiplier = logor multiplier one in
		let del = mul (shift_right_logical (add ndecode one) 1) multiplier in

		pc.{!out} <- del; incr out;

		incr c;

		mb := add (mul pb_local (add n !zmode)) (sub !mb (shift_right (mul pb_local !mb) qbshift));

		(* update mean tracking *)
		if n > n_max_mean_clamp then
			mb := n_mean_clamp_val;

		zmode := zero;

		if ((shift_left !mb mmulshift) < qb) && (!c < num_samples) then begin
			zmode := one;
			let k = (lead !mb) - bitoff + ((to_int !mb + moff) asr mdenshift) in (* asr or lsr? *)
			let mz = logand (sub (shift_left one k) one) wb_local in

			let n = dyn_get bitstream mz k in

			begin try for j = 0 to to_int n - 1 do
				pc.{!out} <- zero; incr out;
				incr c;
			done; with _ -> () end;

			if n >= 65535l then zmode := zero;

			mb := zero;
		end;
	done
