
open Parser

type 'a packet = {
		src : int;
		dst : int;
		len : int;
		checksum : int;
		content : 'a;
	}

let packet content =
	let header = (fun src dst len sum ->
			src, dst, len, sum) >>= word16 >> word16
		>> word16 >> word16 in
	let parser (src, dst, len, sum) = (fun _ content -> {
			src = src;
			dst = dst;
			len = len;
			checksum = sum;
			content = content;
		}) >>= truncate (len - 8) >> content in
	P begin fun input ->
			let Out (args, input) = parse header input in
			parse (parser args) input
	end
