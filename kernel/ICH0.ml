
open PCI
open AudioMixer

(* The 'ICH0' sound driver *)

(* two sets of port i/o spaces *)
type io_space = {
	read8   : int -> int;
	read16  : int -> int;
	read32  : int -> int32;
	write8  : int -> int -> unit;
	write16 : int -> int -> unit;
	write32 : int -> int32 -> unit;
}

let make_io_space addr = {
	read8 = begin fun x -> Asm.in8 (addr + x) end;
	read16 = begin fun x -> Asm.in16 (addr + x) end;
	read32 = begin fun x -> Asm.in32 (addr + x) end;
	write8 = begin fun x -> Asm.out8 (addr + x) end;
	write16 = begin fun x -> Asm.out16 (addr + x) end;
	write32 = begin fun x -> Asm.out32 (addr + x) end;
}

module R = struct
	let reset = 0x00
	let mute = [0x02; 0x04; 0x18]
	let bdl_offset = 0x10
	let last_valid = 0x15
	let current = 0x14
	let control = 0x1B
	let status = 0x16
	let sample_rate = 0x2C
end

let num_buffers = 32
let buffer_size = 32768 (* in samples, which are 16-bit *)

let create device =
	let module C = struct
		open Bigarray
		open BlockIO
	
		(* set up our i/o spaces *)
		let nambar = make_io_space
			(Int32.to_int (read32 device.id 0x10) land lnot 1)
		let nabmbar = make_io_space
			(Int32.to_int (read32 device.id 0x14) land lnot 1)
		
		(* create DMA buffers and buffer descriptor list *)
		let buffers = Array.init num_buffers begin fun _ ->
				Array1.create int8_unsigned c_layout (buffer_size * 2)
			end
		let bdl = Array1.create int32 c_layout (num_buffers * 2)
		
		(* some helper functions rarely used *)
		let get_bit io_space addr bit =
			io_space.read8 addr land (1 lsl bit) <> 0
		let set_bit io_space addr bit =
			io_space.write8 addr (io_space.read8 addr lor (1 lsl bit))
		let clear_bit io_space addr bit =
			io_space.write8 addr (io_space.read8 addr land (lnot (1 lsl bit)))
		
		(* get buffer descriptor pointers *)
		let last_valid () =
			nabmbar.read8 R.last_valid
		let current () =
			nabmbar.read8 R.current
		let next_buffer buffer =
			(buffer + 1) mod num_buffers
		
		(* isr & output routines *)
		let m = Mutex.create()
		let cv = Condition.create()
		
		let isr () =
			if next_buffer (last_valid ()) <> current () then begin
				(* clear the interrupt, by writing '1' to bit 3 *)
				nabmbar.write16 R.status (nabmbar.read16 R.status lor 8);
				Mutex.lock m;
				Condition.signal cv;
				Mutex.unlock m;
			end
		
		let output block_input =
			(* the length = total size of input - current position *)
			let len = Array1.dim block_input.data in
			(*let samples = Array1.dim block_input.data - block_input.pos in*)
			let rec loop () =
				if block_input.pos >= len then begin
					(* we've finished! *)
					()
				end else begin
					(* we can shuffle more data into the card *)
					Mutex.lock m;
					while next_buffer (last_valid ()) = current () do
						(* wait for a free buffer *)
						Condition.wait cv m;
					done;
					Mutex.unlock m;
					(* get the next buffer *)
					let ix = next_buffer (last_valid ()) in
					let buffer = buffers.(ix) in
					(* copy data into the buffer *)
					let size = min (buffer_size * 2) (len - block_input.pos) in
					if size < (buffer_size * 2) then
						BlockIO.blit block_input (Array1.sub buffer 0 size)
					else
						BlockIO.blit block_input buffer;
					(* send the command byte and size (in samples) *)
					bdl.{2*ix+1} <- Int32.logor 0x8000_0000l (Int32.of_int (size lsr 1));
					nabmbar.write8 R.last_valid ix;
					loop ()
				end
			in loop ()
		
		
	end in
	
	(* init the device *)
	for i = 0 to num_buffers - 1 do
		C.bdl.{2*i} <- Asm.address C.buffers.(i);
		C.bdl.{2*i+1} <- Int32.zero
	done;
	
	(* reset PCM out *)
	C.clear_bit C.nabmbar R.control 0;
	C.clear_bit C.nabmbar R.control 4;
	C.clear_bit C.nabmbar R.control 3;
	C.clear_bit C.nabmbar R.control 2;
	C.set_bit C.nabmbar R.control 1;
	while C.get_bit C.nabmbar R.control 1 do () done;
	
	(* reset and unmute the audio device *)
	C.nambar.write16 R.reset 1;
	List.iter begin fun x ->
		C.nambar.write16 x 0
	end R.mute;
	
	(* program the buffer descriptor list *)
	C.nabmbar.write32 R.bdl_offset (Asm.address C.bdl);
	C.nabmbar.write8 R.last_valid (C.nabmbar.read8 R.current);
	
    (* set sample rate to 44100 hertz (more common then default of 48000) *)
    C.nambar.write16 R.sample_rate 44100;
	
	(* register an interrupt handler *)
	Interrupts.create device.request_line C.isr;
	C.set_bit C.nabmbar R.control 4; (* interrupts for buffer completion *)
	Vt100.printf "ich0: on request line %02X\n" device.request_line;
	
	(* start output *)
	C.nabmbar.write8 R.control (C.nabmbar.read8 R.control lor 1);
	
	(* register with the audio mixer *)
	let sample_rate = C.nambar.read16 R.sample_rate in
	Vt100.printf "ich0: sample rate = %d\n" sample_rate;
	AudioMixer.register_device {
		format = 16, sample_rate, 2;
		output = C.output;
	}

let init () =
	DeviceManager.add_driver "Intel ICH0 Sound Card" create 0x8086 0x2415; (* http://svn.berlios.de/wsvn/haiku/haiku/trunk/src/add-ons/kernel/drivers/audio/ac97/ichaudio/ichaudio.c *)
	DeviceManager.add_driver "Intel ICH0 Sound Card" create 0x8086 0x293E
