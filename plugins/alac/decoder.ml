
(* ALAC : Decoder *)

let failf args = Printf.kprintf failwith args

type element
= SCE (* single channel element *)
| CPE (* channel pair element *)
| CCE (* coupling channel element *)
| LFE (* LFE channel element *)
| DSE
| PCE
| FIL
| END

let of_element = function
	| SCE -> 0
	| CPE -> 1
	| CCE -> 2
	| LFE -> 3
	| DSE -> 4
	| PCE -> 5
	| FIL -> 6
	| END -> 7
let to_element x = match x with
	| 0 -> SCE
	| 1 -> CPE
	| 2 -> CCE
	| 3 -> LFE
	| 4 -> DSE
	| 5 -> PCE
	| 6 -> FIL
	| 7 -> END
	| n -> failwith "to_element"

type specific_config = {
	frame_length : int32; (* uint32 *)
	(*compatible_version : int; (* uint8 *) -- has no real use for us *)
	bit_depth : int; (* uint8 *)
	pb : int; (* uint8 *)
	mb : int; (* uint8 *)
	kb : int; (* uint8 *)
	num_channels : int; (* uint8 *)
	max_run : int; (* uint16 *)
	max_frame_bytes : int32; (* uint32 *)
	avg_bit_rate : int32; (* uint32 *)
	sample_rate : int32; (* uint32 *)
}

let print_specific_config cfg =
	Printf.printf (
		"frame length:    %ld\n" ^^
		"bit depth:       %d\n" ^^
		"pb:              %d\n" ^^
		"mb:              %d\n" ^^
		"kb:              %d\n" ^^
		"channels:        %d\n" ^^
		"max run:         %d\n" ^^
		"max frame bytes: %ld\n" ^^
		"avg bitrate:     %ld\n" ^^
		"sample rate:     %ld\n")
	cfg.frame_length cfg.bit_depth cfg.pb cfg.mb cfg.kb cfg.num_channels
	cfg.max_run cfg.max_frame_bytes cfg.avg_bit_rate cfg.sample_rate

let rec init bits =
	bitmatch bits with
	| { _ : 32 : bitstring; "frma" : 32 : string; _ : 32 : bitstring; rest : -1 : bitstring } ->
		(* skip format ('frma') atom if present *)
		init rest
	| { _ : 32 : bitstring; "alac" : 32 : string; _ : 32 : bitstring; rest : -1 : bitstring } ->
		(* skip 'alac' atom header if present *)
		init rest
	| {
		frame_length : 32 : bigendian;
		compatible_version : 8;
		bit_depth : 8;
		pb : 8;
		mb : 8;
		kb : 8;
		num_channels : 8;
		max_run : 16 : bigendian;
		max_frame_bytes : 32 : bigendian;
		avg_bit_rate : 32 : bigendian;
		sample_rate : 32 : bigendian }
		(* ensure version matches *)
		when compatible_version = 0 ->
		(* return the specific_config... the buffers don't matter too much right now *)
		({
			frame_length = frame_length;
			bit_depth = bit_depth;
			pb = pb;
			mb = mb;
			kb = kb;
			num_channels = num_channels;
			max_run = max_run;
			max_frame_bytes = max_frame_bytes;
			avg_bit_rate = avg_bit_rate;
			sample_rate = sample_rate;
		})
	| { _ : -1 : bitstring } -> failwith "alac: missing/invalid cookie"

let fill_element bits =
	let count = match BitBuffer.read_small bits 4 with
	| 15 -> 15 + BitBuffer.read_small bits 8 - 1
	| n -> n
	in
	BitBuffer.advance bits (count * 8)

let data_stream_element bits =
	let _ (* element_instance_tag *) = BitBuffer.read_small bits 4 in
	let data_byte_align_flag = BitBuffer.read_one bits in
	let count = match BitBuffer.read_small bits 8 with
	| 255 -> 255 + BitBuffer.read_small bits 8
	| n -> n
	in
	if data_byte_align_flag <> 0 then BitBuffer.byte_align bits false;
	BitBuffer.advance bits (count * 8)

let zero16 (buffer : ArrayTypes.int16a) num_items stride =
	failwith "alac: shouldn't need; only dealing with stereo files"

(* globals *)

open Bigarray

let config = ref {
	frame_length = 0l;
	bit_depth = 0; pb = 0; mb = 0; kb = 0;
	num_channels = 0; max_run = 0;
	max_frame_bytes = 0l;
	avg_bit_rate = 0l;
	sample_rate = 0l;
}

let mix_buffer_U = ref (Array1.create int32 c_layout 0)
let mix_buffer_V = ref (Array1.create int32 c_layout 0)
let predictor    = ref (Array1.create int32 c_layout 0)
let shift_buffer = ref (Array1.create int16_unsigned c_layout 0)

exception Done

let decode bits (sample_buffer : ArrayTypes.uint8a) num_samples num_channels =
	let shift_bits = ref (BitBuffer.create "" 0) in
	let out_num_samples = ref 0 in
	let coefs_U = Array1.create int16_signed c_layout 32 in
	let coefs_V = Array1.create int16_signed c_layout 32 in
	let mix_bits = ref 0 in
	let mix_res = ref 0 in
	try (*while true do*)
		let pb = !config.pb in
		begin match to_element (BitBuffer.read_small bits 3) with
		| CPE ->
			(* stereo channel pair *)
			let _ (* element_instance_tag *) = BitBuffer.read_small bits 4 in
			(* don't care about active elements *)

			(* read the 12 unused header bits *)
			let unused_header = BitBuffer.read bits 12 in
			(* assert = 0 *)
			assert (unused_header = 0);

			(* read the 1-bit "partial frame" flag, 2-bit "shift-off" flag & 1-bit "escape" flag *)
			let header_byte = BitBuffer.read bits 4 in

			let partial_frame = header_byte lsr 3 in
			let bytes_shifted = (header_byte lsr 1) land 0x3 in
			(* assert != 3 *)
			assert (bytes_shifted <> 3);

			(*let shift = bytes_shifted * 8 in (* unused *)*)
			let escape_flag = header_byte land 0x1 in

			let chan_bits = 16 - (bytes_shifted * 8) + 1 in

			(* check for partial frame length to override requested num_samples *)
			let num_samples = if partial_frame <> 0 then begin
					let override = (BitBuffer.read bits 16) lsl 16 in
					override lor BitBuffer.read bits 16;
				end else num_samples in

			out_num_samples := num_samples;			

			if escape_flag = 0 then begin
				(* compressed frame, read rest of parameters *)
				mix_bits := BitBuffer.read bits 8;
				mix_res := BitBuffer.read bits 8;

				let header_byte = BitBuffer.read bits 8 in
				let mode_U = header_byte lsr 4 in
				let den_shift_U = header_byte land 0xf in

				let header_byte = BitBuffer.read bits 8 in
				let pb_factor_U = header_byte lsr 5 in
				let num_U = header_byte land 0x1f in

				for i = 0 to num_U - 1 do
					coefs_U.{i} <- BitBuffer.read bits 16;
				done;
		
				let header_byte = BitBuffer.read bits 8 in
				let mode_V = header_byte lsr 4 in
				let den_shift_V = header_byte land 0xf in

				let header_byte = BitBuffer.read bits 8 in
				let pb_factor_V = header_byte lsr 5 in
				let num_V = header_byte land 0x1f in

				for i = 0 to num_V - 1 do
					coefs_V.{i} <- BitBuffer.read bits 16;
				done;
		
				(* if shift active, skip the interleaved shifted values, but remember where they start *)
				if bytes_shifted <> 0 then begin
					shift_bits := BitBuffer.copy bits;
					BitBuffer.advance bits (bytes_shifted * 8 * 2 * num_samples);
				end;

				(* decompress and run predictor for "left" channel *)
				let ag_params = AdaptiveGolomb.make_params !config.mb ((pb * pb_factor_U) / 4) !config.kb num_samples num_samples !config.max_run in
				AdaptiveGolomb.dyn_decomp ag_params bits !predictor num_samples chan_bits;

				if mode_U = 0 then begin
					DynamicPredictor.unpc_block !predictor !mix_buffer_U num_samples coefs_U num_U chan_bits den_shift_U;
				end else begin
					(* the special "num_active = 31" mode can be done in-place *)
					DynamicPredictor.unpc_block !predictor !predictor num_samples coefs_U 31 chan_bits 0;
					DynamicPredictor.unpc_block !predictor !mix_buffer_U num_samples coefs_U num_U chan_bits den_shift_U;
				end;

				(* decompress and run predictor for "right" channel -- U => V *)
				let ag_params = AdaptiveGolomb.make_params !config.mb ((pb * pb_factor_V) / 4) !config.kb num_samples num_samples !config.max_run in
				AdaptiveGolomb.dyn_decomp ag_params bits !predictor num_samples chan_bits;

				if mode_V = 0 then begin
					DynamicPredictor.unpc_block !predictor !mix_buffer_V num_samples coefs_V num_V chan_bits den_shift_V;
				end else begin
					DynamicPredictor.unpc_block !predictor !predictor num_samples coefs_V 31 chan_bits 0;
					DynamicPredictor.unpc_block !predictor !mix_buffer_V num_samples coefs_V num_V chan_bits den_shift_V;
				end;
			end else begin
				(* uncompressed frame, copy data into the mix buffers to use common output code *)
				let chan_bits = 16 in (* !config.bit_depth *)
				let shift = 32 - chan_bits in
				(* if chan_bits <= 16 *)
				for i = 0 to num_samples - 1 do
					let value = Int32.of_int (BitBuffer.read bits chan_bits) in
					let value = Int32.shift_right (Int32.shift_left value shift) shift in
					!mix_buffer_U.{i} <- value;

					let value = Int32.of_int (BitBuffer.read bits chan_bits) in
					let value = Int32.shift_right (Int32.shift_left value shift) shift in
					!mix_buffer_V.{i} <- value;
				done;

				(* bits1 & bits2 serve no useful purpose *)
				mix_bits := 0;
				mix_res := 0;
				(* bytes_shifted <- 0, if escape_flag = 0 && bytes_shifted <> 0 *)
			end;

			(* now read the shifted values into the shift buffer *)
			(* if escape_flag <> 0, then don't need to shift *)
			if escape_flag = 0 && bytes_shifted <> 0 then begin
				let shift = bytes_shifted * 8 in
				(* assert <= 16 *)
				assert (shift <= 16);

				for i = 0 to num_samples - 1 do
					!shift_buffer.{i * 2}     <- BitBuffer.read !shift_bits shift;
					!shift_buffer.{i * 2 + 1} <- BitBuffer.read !shift_bits shift;
				done
			end;

			(* un-mix the data and convert to output format *)
			(* - note that mix_res = 0 means just interleave so we use that path for uncompressed frames *)
			let out16 = BigarrayUtils.uint8_to_int16 sample_buffer in
			Matrix.unmix16 !mix_buffer_U !mix_buffer_V out16 num_channels num_samples !mix_bits !mix_res;
		| DSE ->
			(* data stream element -- parse but ignore *)
			data_stream_element bits
		| FIL ->
			(* fill element -- parse but ignore *)
			fill_element bits
		| END ->
			BitBuffer.byte_align bits false;
			raise Done
		| x -> failf "unexpected frame element: %d%!" (of_element x)
		end;
	(*done;*) `ok !out_num_samples with Done -> `ok !out_num_samples | ex -> `fail (!out_num_samples,ex)
