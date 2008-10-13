
(* SB16 driver *)

module C = struct
	let base  = 0x220
	let irq   = 5
	let dma8  = 1
	let dma16 = 5
end

module R = struct
	let mixer  = 0x04
	let data   = 0x05
	let reset  = 0x06
	let read   = 0x0A
	let write  = 0x0C
	let status = 0x0E
	let ack16 = 0xF
end

module M = struct
	let master_left  = 0x30
	let master_right = 0x31
	let gain_left    = 0x41
	let gain_right   = 0x42
end

exception Timeout

let out8 o v = Asm.out8 (C.base + o) v
let in8 o = Asm.in8 (C.base + o)

let spin o p =
	let rec spin lim =
		if lim = 100_000 then raise Timeout
		else if p (in8 o) then spin (lim + 1)
		else ()
	in spin 0

let reset () =
	out8 R.reset 1;
	out8 R.reset 0;
	spin R.status (fun x -> x land 0x80 = 0);
	spin R.read (fun x -> x <> 0xAA)

let read () =
	spin R.status (fun x -> x land 0x80 = 0);
	in8 R.data

let write v =
	spin R.write (fun x -> x land 0x80 = 1);
	out8 R.write v

let write_mixer m v =
	out8 R.mixer m;
	out8 R.data v

let read_mixer m =
	out8 R.mixer m;
	in8 R.data

let set_sample_rate hertz =
	write 0x41;
	write (hertz lsr 8);
	write (hertz land 0xFF)

let len = ref 0

let block = DMA.allocate ()

let handler () =
	(*write 0x14;
	write ((!len - 1) land 0xFF);
	write ((!len - 1) asr 8);
	DMA.start C.dma8 block 0b0001_1000;*)
	let _ = read_mixer 0x82 in
	(* b0 = sb-midi / 8bit dma-mode digital sound *)
	(* b1 = 16 bit dma mode digital sound *)
	(* b2 = mpu-401 *)
	(* when b0 = 1 *)
	ignore (in8 R.status)
	(*(* when b1 = 1 *)
	in8 (base + 0x0F)
	(* when b2 = 1 *)
	in8 (midi_base)*)
	

let output sample =
	reset ();
	Interrupts.create C.irq handler;
	write 0xD1; (* turn on DAC speaker *)
	write_mixer M.master_left 0xF8; (* max *)
	write_mixer M.master_right 0xF8; (* max *)
	write_mixer M.gain_left 0xC0; (* max *)
	write_mixer M.gain_right 0xC0;
	(*let mode = 0b0001_1000 in (* demand, address inc, non-auto *)*)
	let mode = 0b0101_1000 in (* single, auto *)
	DMA.start_transfer C.dma8 block 0x45
	;
	set_sample_rate 11025;
	let length = Array.length sample in
	for i = 0 to length - 1 do
		block.{i} <- sample.(i);
	done;
	(* 0x14 = 0001_0100 *)
	(*write 0x14; (* DMA mode; 8bit unsigned mono *)*)
	(*write 0x48;
	write ((length - 1) land 0xFF);
	write ((length - 1) asr 8);
	write 0x1C; (* 8bit PCM output; auto-initialized DMA *)*)
	(* single-cycle 8bit PCM output *)
	len := length;
	write 0xC0; (* 8-bit output *)
	write 0x00; (* 8-bit mono unsigned PCM *)
	write 0xFF; (*((length - 1) land 0xFF);*)
	write 0xFF; (*((length - 1) asr 8);*)
	()
