
(* ALAC : Matrix Decoding *)


open ArrayTypes

(* u : int32, v : int32, out : int16 ... *)
let unmix16 (u : int32a) (v : int32a) (out : int16a) stride num_samples mixbits = function (* mixres *)
	| 0 -> (* the else branch *)
		(* conventional separated stereo *)
		for j = 0 to num_samples - 1 do
			out.{j * stride} <- Int32.to_int u.{j}; (* cast from 32 to 16 bit *)
			out.{j * stride + 1} <- Int32.to_int v.{j}; (* cast from 32 to 16 bit *)
		done
	| mixres ->
		(* matrixed stereo *)
		for j = 0 to num_samples - 1 do
			let l = Int32.sub (Int32.add u.{j} v.{j}) (Int32.shift_right_logical (Int32.mul (Int32.of_int mixres) v.{j}) mixbits) in
			let r = Int32.sub l v.{j} in

			out.{j * stride} <- Int32.to_int l; (* cast from 32 to 16 bit *)
			out.{j * stride + 1} <- Int32.to_int r; (* cast from 32 to 16 bit *)
		done

let unmix20 (u : int32a) (v : int32a) (out : uint8a) stride num_samples mixbits = function (* mixres *)
	| 0 ->
		(* conventional separated stereo *)
		let op = ref 0 in
		for j = 0 to num_samples - 1 do
			let value = Int32.shift_left u.{j} 4 in
			out.{!op+2} <- (Int32.to_int (Int32.shift_right_logical value 16)) land 0xFF;
			out.{!op+1} <- (Int32.to_int (Int32.shift_right_logical value 8)) land 0xFF;
			out.{!op} <- (Int32.to_int value) land 0xFF;
			op := !op + 3;

			let value = Int32.shift_left v.{j} 4 in
			out.{!op+2} <- (Int32.to_int (Int32.shift_right_logical value 16)) land 0xFF;
			out.{!op+1} <- (Int32.to_int (Int32.shift_right_logical value 8)) land 0xFF;
			out.{!op} <- (Int32.to_int value) land 0xFF;
			op := !op + ((stride - 1) * 3);
		done
	| mixres ->
		(* matrixed stereo *)
		let op = ref 0 in
		for j = 0 to num_samples - 1 do
			let l = Int32.sub (Int32.add u.{j} v.{j}) (Int32.shift_right_logical (Int32.mul (Int32.of_int mixres) v.{j}) mixbits) in
			let r = Int32.sub l v.{j} in

			let l = Int32.shift_left l 4 in
			let r = Int32.shift_left r 4 in

			out.{!op+2} <- (Int32.to_int (Int32.shift_right_logical l 16)) land 0xFF;
			out.{!op+1} <- (Int32.to_int (Int32.shift_right_logical l 8)) land 0xFF;
			out.{!op} <- (Int32.to_int l) land 0xFF;
			op := !op + 3;

			out.{!op+2} <- (Int32.to_int (Int32.shift_right_logical r 16)) land 0xFF;
			out.{!op+1} <- (Int32.to_int (Int32.shift_right_logical r 8)) land 0xFF;
			out.{!op} <- (Int32.to_int r) land 0xFF;
			op := !op + ((stride - 1) * 3);
		done

let unmix24 (u : int32a) (v : int32a) (out : uint8a) stride num_samples mixbits mixres (shift_uv : uint16a) bytes_shifted =
	failwith "unmix24"

let unmix32 (u : int32a) (v : int32a) (out : int32a) stride num_samples mixbits mixres (shift_uv : uint16a) bytes_shifted =
	failwith "unmix32"

let copy_predictor_to_24 (inp : int32a) (out : uint8a) stride num_samples =
	failwith "copy_predictor_to_24"

let copy_predictor_to_24_shift (inp : int32a) (shift : uint16a) (out : uint8a) stride num_samples bytes_shifted =
	failwith "copy_predictor_to_24_shift"

let copy_predictor_to_20 (inp : int32a) (out : uint8a) stride num_samples =
	failwith "copy_predictor_to_20"

let copy_predictor_to_32 (inp : int32a) (out : int32a) stride num_samples =
	for i = 0 to num_samples - 1 do
		out.{i*stride} <- inp.{i}
	done

let copy_predictor_to_32_shift (inp : int32a) (shift : uint16a) (out : int32a) stride num_samples bytes_shifted =
	let shift_val = bytes_shifted * 8 in
	for j = 0 to num_samples - 1 do
		out.{j*stride} <- Int32.logor (Int32.shift_left inp.{j} shift_val) (Int32.of_int shift.{j});
	done
