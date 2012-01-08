
(* ALAC : Dynamic Predictor decoding *)

open Bigarray

open ArrayTypes

module Printf = struct
	let printf args = Printf.kprintf ignore args
end

(* int32_t *pc1, int32_t *out, int32_t num, int16_t *coefs, int32_t numactive, uint32_t chanbits, uint32_t denshift *)

(* returns nothing; output goes into out *)

(*
uint32_t chanshift = 32 - chanbits;
int32_t denhalf = 1 << (denshift - 1);

out[0] = pc1[0];
if (numactive == 0)
{
	if ((num > 1) && (pc1 != out))
		memcpy(&out[1], &pc1[1], (num - 1) * sizeof(int32_t));
	return;
}
if (numactive == 31)
{
	int32_t prev = out[0];
	for (j = 1; j < num; j++)
	{
		int32_t del = pc1[j] + prev;
		prev = (del << chanshift) >> chanshift;
		out[j] = prev;
	}
	return;
}

for (j = 1; j <= numactive; j++)
{
	int32_t del = pc1[j] + out[j-1];
	out[j] = (del << chanshift) >> chanshift;
}

int32_t lim = numactive + 1;

/* skip numactive optimisations for now... */

for (j = lim; j < num; j++)
{
	int32_t sum1 = 0;
	int32_t *pout = out + j - 1;
	int32_t top = out[j-lim];

	for (k = 0; k < numactive; k++)
		sum1 += coefs[k] * (pout[-k] - top);

	int32_t del = pc1[j];
	int32_t del0 = del;
	int32_t sg = sign_of_int(del); /* inlined */
	del += top + ((sum1 + denhalf) >> denshift);
	out[j] = (del << chanshift) >> chanshift;

	if (sg > 0)
	{
		for (k = (numactive - 1); k >= 0; k--)
		{
			int32_t dd = top - pout[-k];
			int32_t sgn = sign_of_int(dd); /* inlined */
			coefs[k] -= sgn;
			del0 -= (numactive - k) * ((sgn * dd) >> denshift);
			if (del0 <= 0)
				break;
		}
	}
	else if (sg < 0)
	{
		for (k = (numactive - 1); k >= 0; k--)
		{
			int32_t dd = top - pout[-k];
			int32_t sgn = sign_of_int(dd); /* inlined */
			coefs[k] += sgn;
			del0 -= (numactive - k) * ((-sgn * dd) >> denshift);
			if (del0 >= 0)
				break;
		}
	}
}
*)

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
Printf.printf "loop1:\n";
		for j = 1 to numactive do
			let del = Int32.add pc1.{j} out.{j-1} in
			out.{j} <- Int32.shift_right (Int32.shift_left del chanshift) chanshift;
Printf.printf "%08lx " out.{j};
		done;
Printf.printf "\n";

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
