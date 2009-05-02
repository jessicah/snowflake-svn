
(* AudioMixer *)

(* Provides an interface to a sound device *)

type bit_rate
	= Bits8
	| Bits16

type sample_rate
	= KHertz11
	| KHertz22
	| KHertz44
	| KHertz48

type format = int * int * int

module Wave = struct
	open IO
	
	type t = {
		channels: int;
		samples_per_sec: int;
		avg_bytes_per_sec: int;
		block_align: int;
		bits_per_sec: int;
		samples: int;
		input: input;
	}
	
	type chunk = {
		id : string;
		length : int;
	}
	
	let read_chunk ic =
		let id = really_nread ic 4 in
		let length = read_i32 ic in
		{ id = id; length = length; }
	
	let read ic =
		let riff = really_nread ic 4 in
		ignore (really_nread ic 4);
		let wave = really_nread ic 4 in
		assert (riff = "RIFF" && wave = "WAVE");
		let format_chunk = read_chunk ic in
		let chunk = input_string (really_nread ic format_chunk.length) in
		assert (format_chunk.id = "fmt ");
		let format = read_ui16 chunk in
		assert (format = 1);
		let channels = read_ui16 chunk in
		let samples_per_sec = read_i32 chunk in
		let avg_bytes_per_sec = read_i32 chunk in
		let block_align = read_ui16 chunk in
		let bits_per_sec = read_ui16 chunk in
		let rec loop chunk =
			if chunk.id <> "data"
			then begin
				ignore (really_nread ic chunk.length);
				loop (read_chunk ic)
			end else {
				channels = channels;
				samples_per_sec = samples_per_sec;
				avg_bytes_per_sec = avg_bytes_per_sec;
				block_align = block_align;
				bits_per_sec = bits_per_sec;
				samples = chunk.length / (channels * bits_per_sec / 8);
				(*read_sample = begin function
					| Some rate -> begin
						match bits_per_sec with
						| 16 ->
					| None -> begin
						match bits_per_sec with
						| 16 -> read_i16 ic (* only signed 16-bit even *)
						| _ -> failwith "unsupported bit rate"
					end
				end;*)
				input = ic;
			}
		in loop (read_chunk ic)
end

open Wave

type output = {
	format: format;
	output: (unit -> int) -> int -> unit;
}

(* resampling *)

let resample wave new_rate =
	let ratio = float new_rate /. float wave.samples_per_sec in
	let scale v = int_of_float (float v /. ratio) in
	let last_samples = Array.make wave.channels 0 in
	let last_t = ref 0 in
	let last_c = ref 0 in
	let length = int_of_float (float wave.samples *. ratio) in
	let read () =
		let chan = !last_c in
		incr last_c;
		if !last_c = 1 then
			incr last_t;
		if !last_c = wave.channels then
			last_c := 0;
		if scale !last_t = scale (!last_t - 1) && !last_t <> 1 then
			last_samples.(chan)
		else begin
			if chan = 0 then begin
				for i = 0 to wave.channels - 1 do
					last_samples.(i) <- IO.read_i16 wave.input;
				done;
			end;
			last_samples.(chan);
		end
	in read, length

(* Pretty much allows for only one audio device atm *)

let play_to device wave =
	match device.format with
	| (bits,hertz,chans)
		when bits = wave.bits_per_sec && bits = 16
		&& hertz = wave.samples_per_sec
		&& chans = wave.channels ->
		(* no conversion required; push data directly to the driver *)
		(*let fill_buffer buffer samples =
			for i = 0 to samples / chans - 1 do
				for j = 0 to chans - 1 do
					buffer.{i,j} <- wave.read_sample None;
				done;
			done
		in*) (* pass to driver so it can grab more data when it wants it *)
		device.output (fun () -> IO.read_i16 wave.input) wave.samples
	| (bits,hertz,chans)
		when bits = wave.bits_per_sec && bits = 16
		&& chans = wave.channels ->
		(* only sample rate conversion needed *)
		let (read, length) = resample wave hertz in
		device.output read length
	| _ ->
		(* wave and device formats don't match *)
		failwith "input format not match output format"

let device = ref None

let play wave = match !device with
	| None -> failwith "No audio device present"
	| Some device -> play_to device wave

let stop () = failwith "not implemented"

let register_device outputDevice =
	device := Some outputDevice
