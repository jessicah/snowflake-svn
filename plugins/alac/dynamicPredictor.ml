
(* ALAC : Dynamic Predictor decoding *)

open Bigarray

open ArrayTypes

let sign_of_int i =
	let negishift = Int32.shift_right_logical (Int32.neg i) 31 in
	Int32.logor negishift (Int32.shift_right i 31)

let rec loop_while p f = function
	| n when n >= 0 -> if p (f n) then loop_while p f (n-1)
	| _ -> ()

let unpc_block (pc1 : int32a) (out : int32a) num (coefs : int16a) numactive chanbits denshift =
	let chanshift = 32 - chanbits in
	let denhalf = 1 lsl (denshift - 1) in

	out.{0} <- pc1.{0};

	if numactive = 0 then begin
		if num > 1 then
			Array1.blit (Array1.sub pc1 1 (num-1)) (Array1.sub out 1 (num-1)) (* copy pc1[1 .. num-1] to out[1 .. num-1] *)
	end else if numactive = 31 then begin
		let prev = ref out.{0} in
		for j = 1 to num - 1 do
			let del = Int32.add pc1.{j} !prev in
			prev := Int32.shift_right (Int32.shift_left del chanshift) chanshift;
			out.{j} <- !prev;
		done
	end else begin
		for j = 1 to numactive do
			let del = Int32.add pc1.{j} out.{j-1} in
			out.{j} <- Int32.shift_right (Int32.shift_left del chanshift) chanshift;
		done;

		let lim = numactive + 1 in

		for j = lim to num - 1 do
			let sum1 = ref Int32.zero in
			let top = out.{j-lim} in

			for k = 0 to numactive - 1 do
				sum1 := Int32.add !sum1 (Int32.mul (Int32.of_int (coefs.{k})) (Int32.sub out.{j-1-k} top)); (***)
			done;

			let del = pc1.{j} in
			let del0 = ref pc1.{j} in
			let sg = sign_of_int del in
			let del = Int32.add del (Int32.add top (Int32.shift_right (Int32.add !sum1 (Int32.of_int denhalf)) denshift)) in
			out.{j} <- Int32.shift_right (Int32.shift_left del chanshift) chanshift;

			if sg > 0l then begin
				loop_while (fun _ -> !del0 > 0l) (fun k ->
					let dd = Int32.sub top out.{j-1-k} in
					let sgn = sign_of_int dd in
					coefs.{k} <- coefs.{k} - (Int32.to_int sgn); (***)
					del0 := Int32.sub !del0 (Int32.mul (Int32.of_int (numactive - k)) (Int32.shift_right (Int32.mul sgn dd) denshift));
					) (numactive - 1);
			end else if sg < 0l then begin
				loop_while (fun _ -> !del0 < 0l) (fun k ->
					let dd = Int32.sub top out.{j-1-k} in
					let sgn = sign_of_int dd in
					coefs.{k} <- coefs.{k} + Int32.to_int sgn; (***)
					del0 := Int32.sub !del0 (Int32.mul (Int32.of_int (numactive - k)) (Int32.shift_right (Int32.mul (Int32.neg sgn) dd) denshift));
					) (numactive - 1);
			end;
		done;
	end
