
(* a lil music player -- eventually would have a file system for this to work with *)

open Arg
open Shell
open Bigarray

(* raised when a decoder is given a file it doesn't support *)
exception Not_compatible

type decoder = {
	openfile : string -> in_channel * BlockIO.input; (* bigarray *)
	decode   : in_channel -> BlockIO.input -> bool; (* if there more to decode *)
}

let decoders : decoder list ref = ref []

let register_decoder decoder =
	decoders := decoder :: !decoders

(* based loosely on the streaming music player code *)
let rec play_file filename = function
	| [] -> failwith "no valid decoder available"
	| decoder :: decoders ->
		try
			let handle, blockio = decoder.openfile filename in
			AudioMixer.play (AudioMixer.Wave.read blockio);
			let more = decoder.decode handle blockio in
			let more = ref more in
			while !more do
				(* refills the decode_buffer, so reset blockio position to 0 *)
(*Debug.printf ".";*)
				AudioMixer.play_raw blockio;(* just decode for now, to see how fast it is... *)
(*Debug.printf "+";*)
				blockio.BlockIO.pos <- 0;
				let more' = decoder.decode handle blockio in
				more := more';
			done;
Debug.printf "!";
		with
			| Not_compatible -> play_file filename decoders
			| ex ->
				Printexc.print_backtrace stderr;
				play_file filename decoders
				(*raise ex (* just re-raise anything we don't know about *)*)

let play_file filename =
	Printexc.record_backtrace true;
	play_file filename !decoders;
	Printexc.record_backtrace false

(* hack around linking *)

let init () =
	add_command "musicplayer" ignore ~anon:play_file [];
	let s_buffer = String.make (4096 lsl 6) '\000' in
	let openfile filename =
		let ic = open_in_bin filename in
		let buffer = Array1.create int8_unsigned c_layout (4096 lsl 6) in
		let blockio = BlockIO.make buffer in
		really_input ic s_buffer 0 (4096 lsl 6);
		BlockIO.blit_from_string s_buffer blockio;
		blockio.BlockIO.pos <- 0;
		ic, blockio
	in
	let decode ic blockio =
		really_input ic s_buffer 0 (4096 lsl 6);
		blockio.BlockIO.pos <- 0;
		BlockIO.blit_from_string s_buffer blockio;
		blockio.BlockIO.pos <- 0;
		true
	in register_decoder { openfile = openfile; decode = decode }
