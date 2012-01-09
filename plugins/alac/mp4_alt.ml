
type box = {
	kind : string;
	offset : int;
	size : int;
}

let input_int32 ic =
	let a = input_byte ic in
	let b = input_byte ic in
	let c = input_byte ic in
	let d = input_byte ic in
	(a lsl 24) lor (b lsl 16) lor (c lsl 8) lor d

let input_int16 ic =
	let a = input_byte ic in
	let b = input_byte ic in
	(a lsl 8) lor b

let skip ic num =
	seek_in ic (pos_in ic + num)

let box ic offset =
	seek_in ic offset;
	let size = input_int32 ic in
	let kind = String.make 4 '\000' in
	really_input ic kind 0 4;
	if size = 0 then failwith "large box"
	else if size = 1 then failwith "rest of file box"
	else { kind; offset = pos_in ic; size }

let rec find_box ic kind offset limit =
	if offset >= limit then raise Not_found
	else begin
		let b = box ic offset in
		if b.kind = kind then b
		else find_box ic kind (offset + b.size) limit
	end

let rec get_box path ic box = match path with
	| [] -> box
	| k :: kinds ->
		get_box kinds ic (find_box ic k box.offset (box.offset + box.size))

let openfile ic =
	let filebox = { kind = "    "; offset = 0; size = in_channel_length ic } in
	let ftyp = box ic 0 in
	if ftyp.kind <> "ftyp" then failwith "not an mp4 container";
	(* already here, but for sanity's sake *)
	seek_in ic ftyp.offset;
	really_input ic ftyp.kind 0 4;
	if ftyp.kind <> "M4A " then failwith "not an audio file";
	let stsd = get_box ["moov";"trak";"mdia";"minf";"stbl";"stsd"] ic filebox in
	let mdat = get_box ["mdat"] ic filebox in
	(* dump stsd data *)
	seek_in ic (stsd.offset + 4);
	let num_entries = input_int32 ic in
	skip ic 4;
	really_input ic stsd.kind 0 4;
	skip ic 6;
	let x = input_int16 ic in
	skip ic 8;
	let num_channels = input_int16 ic in
	let bits_per_channel = input_int16 ic in
	skip ic 4;
	let sample_rate = input_int16 ic in
	let cookie_offset = pos_in ic + 2 in
	(* we only care about 16-bit stereo @ 44100Hz *)
	assert (num_entries = 1 && stsd.kind = "alac" && x = 1 && num_channels = 2 && bits_per_channel = 16 && sample_rate = 44100);
	Printf.printf "cookie @ %d bytes into file\n" cookie_offset;
	Printf.printf "mdat data starts at %d bytes into file\n" mdat.offset;
	(* return cookie 'box' & the mdat box *)
	{ kind = "kuki"; offset = cookie_offset; size = stsd.size - cookie_offset + stsd.offset }, mdat
