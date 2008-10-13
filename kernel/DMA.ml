
(* Direct Memory Access *)

open Bigarray

external get_dma_region : unit -> (int, int8_unsigned_elt, c_layout) Array1.t
	= "get_dma_region"

let lo_byte x = x land 0xFF
let hi_byte x = x asr 8

let mask_reg   = [| 0x0A; 0x0A; 0x0A; 0x0A; 0xD4; 0xD4; 0xD4; 0xD4 |]
let mode_reg   = [| 0x0B; 0x0B; 0x0B; 0x0B; 0xD6; 0xD6; 0xD6; 0xD6 |]
let clear_reg  = [| 0x0C; 0x0C; 0x0C; 0x0C; 0xD8; 0xD8; 0xD8; 0xD8 |]

let page_port  = [| 0x87; 0x83; 0x81; 0x82; 0x8F; 0x8B; 0x89; 0x8A |]
let addr_port  = [| 0x00; 0x02; 0x04; 0x06; 0xC0; 0xC4; 0xC8; 0xCC |]
let count_port = [| 0x01; 0x03; 0x05; 0x07; 0xC2; 0xC6; 0xCA; 0xCE |]

let allocate () =
	get_dma_region ()

let start_transfer channel data mode =
	let is_16_bit = channel >= 4 in
	let channel4 = channel mod 4 in
	let mode = mode lor channel4 in
	Asm.out8 mask_reg.(channel) (0x04 lor channel4);
	Asm.out8 clear_reg.(channel) 0x00;
	Asm.out8 mode_reg.(channel) mode;
	(* get data *)
	let address = Asm.address data in
	let length =
		if is_16_bit then Array1.dim data / 2 - 1
		else Array1.dim data - 1
	in
	(* prepare transfer *)
	Asm.out8 addr_port.(channel) 0;
	Asm.out8 addr_port.(channel) 0;
	(* load data *)
	Asm.out8 count_port.(channel) (lo_byte length);
	Asm.out8 count_port.(channel) (hi_byte length);
	Asm.out8 page_port.(channel) (Int32.to_int (Int32.div address 0x1_0000l));
	(* unmask channel *)
	Asm.out8 mask_reg.(channel) channel4

let stop_transfer channel =
	let channel4 = channel mod 4 in
	Asm.out8 mask_reg.(channel) (0x04 lor channel4);
	Asm.out8 clear_reg.(channel) 0x00;
	Asm.out8 mask_reg.(channel) channel4
