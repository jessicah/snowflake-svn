
open Packet

type parser_buffer
	= ByteAligned of inpacket
	| Unaligned of int * inpacket

type 'a output = Out of 'a * parser_buffer

type 'a parser = P of (parser_buffer -> 'a output)

exception Invalid_packet
exception End_of_packet
exception Check_failure

let empty_in =
	ByteAligned {
			buffer = [| |];
			from = 0;
			len = 0;
		}

let parse (P p) input = p input

let parse_packet parser packet =
	match parse parser (ByteAligned packet) with
		| Out (result, ByteAligned rest)
				when rest.len = 0 -> result
		| _ -> raise Invalid_packet

let succ' p =
	{ p with from = p.from + 1; len = p.len - 1 }

let succ = function
	| ByteAligned p -> ByteAligned (succ' p)
	| Unaligned (o, p) -> Unaligned (o, succ' p)

let (>>=) f p = P begin fun input ->
		let Out (x, input) = parse p input in
		Out (f x, input)
	end

let (>>) p1 p2 = P begin fun input ->
		let Out (f, input) = parse p1 input in
		let Out (x, input) = parse p2 input in
		Out (f x, input)
	end

let (>>!) p1 p2 = P begin fun input ->
		let Out (x, input) = parse p1 input in
		if x = true then
			parse p2 input
		else
			raise Invalid_packet
	end

let remainder = P begin function
		| ByteAligned p ->
				Out (to_array p, empty_in)
		| _ -> raise Invalid_packet
	end

let truncate len = P begin function
		| ByteAligned p ->
					Out ((), ByteAligned { p with len = min p.len len })
		| _ -> raise Invalid_packet
	end

(** [bind], [return] and [guard] give us what we need to be
    able to do monadic parsing *)
let bind p f = P begin fun input ->
		let Out (x, input) = parse p input in
		parse (f x) input
	end

let return x = P begin fun input ->
		Out (x, input)
	end

let guard condition =
	if condition then return () else raise Check_failure

let onebit = P begin fun input ->
		let testbit p o =
			(byte p 0) land (1 lsl o) <> 0 in
		let unaligned o p =
			if p.len = 0 then raise End_of_packet
			else Out (testbit p o, Unaligned (o+1, p))
		in match input with
			| Unaligned (7, p) ->
					if p.len = 0 then raise End_of_packet
					else Out (testbit p 0, ByteAligned (succ' p))
			| Unaligned (o, p) -> unaligned o p
			| ByteAligned p -> unaligned 0 p
	end

let word8 = P begin function
		| ByteAligned p as i ->
				if p.len = 0 then raise End_of_packet
				else Out (byte p 0, succ i)
		| Unaligned (_, p) as i ->
				if p.len < 2 then raise End_of_packet
				else Out (byte p 0, succ i)
	end

let word16 =
	(fun x y -> (x lsl 8) lor y) >>= word8 >> word8

let word32 =
	(fun x y -> (x lsl 16) lor y) >>= word16 >> word16

let iword32 =
	(fun x y -> Int32.logor (Int32.shift_left (Int32.of_int x) 16) (Int32.of_int y))
		>>= word16 >> word16

		
let rec bytes n =
	if n = 0 then return []
	else (fun x y -> x :: y) >>= word8 >> bytes (n-1)

let bits n =
	if n = 0 then return 0
	else
		let
			bytecnt = n / 8 and bitcnt = n mod 8
		and
			make x xs =
				List.fold_left (fun x y -> (x lsl 8) lor y) 0 (x :: xs)
		and
			bits n = P begin function
				| ByteAligned p ->
						if p.len = 0 then raise End_of_packet
						else Out ((byte p 0) asr (8-n), Unaligned (n, p))
				| Unaligned (offset, p) ->
						if p.len = 0 then raise End_of_packet;
						let offset' = offset + n in
						let mask n = (1 lsl n) - 1 in
						if offset' < 8 then
							Out ((byte p 0) asr ((8-offset') land (mask n)), Unaligned (offset', p))
						else if offset' = 8 then
							Out ((byte p 0) land (mask n), ByteAligned (succ' p))
						else if p.len = 1 then raise End_of_packet
						else
							let o = offset - 8 in
							let out =
								(((byte p 0) land (mask (8-offset-n))) lsl o) lor
								((byte p 0) asr (8-o))
							in Out (out, Unaligned (o, succ' p))
		end
	in if bitcnt = 0 then
		make >>= word8 >> bytes (bytecnt - 1)
	else
		make >>= bits bitcnt >> bytes bytecnt

let check8 w8 = ((=) w8) >>= word8

let check16 w16 = ((=) w16) >>= word16

module Unparse = struct

	type unp = {
			cnt : int;
			obytes : int list;
			outpacket : outpacket;
		}
	
	type unparse = unp -> unp
	
	type 'a unparser = U of ('a -> unparse)

	let empty_out = {
			outlen = 0;
			chunks = []
		}
	
	let unparse (U u) x =
		let empty = {
				cnt = 0;
				obytes = [];
				outpacket = empty_out
			}
		in u x empty
	
	(*let unparse_value unparser value =
		match unparse unparser (ByteAligned packet) with
			| Out (result, ByteAligned rest)
					when rest.len = 0 -> result
			| _ -> raise Invalid_packet*)
	let rec do_unparse u x =
		flush (unparse u x)
	and add_chunk a p = {
			outlen = Array.length a + p.outlen;
			chunks = a :: p.chunks
		}
	and flush u = add_chunk (Array.of_list u.obytes) u.outpacket
	
	let word8 = U (fun w u -> { u with cnt = u.cnt + 1; obytes = w :: u.obytes })
	
	let (+++) f g x = f (g x)
	
	let tuple (U a) (U b) = U (fun (x,y) -> fun z -> a x (b y z))
	let triple (U a) (U b) (U c) = U (fun (x,y,z) -> fun k -> a x (b y (c z k)))
	
	let (>>) = tuple
	
	let (>>=) (U u) f = U (fun x -> u (f x))
	
	type addr = Addr of (int * int * int * int * int * int)
	
	let unmake (Addr (a,b,c,d,e,f)) = ((a,b,c),(d,e,f))
	
	let addr = triple word8 word8 word8 >> triple word8 word8 word8 >>= unmake
	
	let x = triple word8 word8 word8
	let y = fun x a z -> tuple (triple x a z) word8
	let z = y word8 word8 word8
	let z2 = (triple word8 word8 word8) >> word8
	
	let result = do_unparse z2 ((1,3,4), 2)
	
	let print_chunk () chunk =
		let b = Buffer.create (Array.length chunk) in
		Array.iter (fun i -> Buffer.add_string b (Printf.sprintf "%02X " i)) chunk;
		Buffer.contents b
	
	let list_chunks () chunk_list =
		let string_list = List.map (Printf.sprintf "%a" print_chunk) chunk_list in
		String.concat " @ " string_list
	
	let a = Addr (0x00, 0x00, 0xCA, 0xFE, 0xBA, 0xBE)
	
	let () =
		let result = do_unparse addr a in
		Vt100.printf "Unparsing ethernet address resulted in outpacket: outlen = %d, chunks = %a\r\n"
		result.outlen list_chunks result.chunks
	
	let () = Vt100.printf "Unparsing ((1,3,4),2) resulted in outpacket: outlen = %d, chunks = %a\r\n"
		result.outlen list_chunks result.chunks

end
