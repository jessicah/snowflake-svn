
open PCI
open AudioMixer

type io_space = {
	read8   : int -> int;
	read16  : int -> int;
	read32  : int -> int32;
	write8  : int -> int -> unit;
	write16 : int -> int -> unit;
	write32 : int -> int32 -> unit;
}

let make_io_space resource = {
	read8 = AddressSpace.read8 resource;
	read16 = AddressSpace.read16 resource;
	read32 = AddressSpace.read32 resource;
	write8 = AddressSpace.write8 resource;
	write16 = AddressSpace.write16 resource;
	write32 = AddressSpace.write32 resource;
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
	
		(* set up our i/o spaces *)
		let nambar = make_io_space device.resources.(0)
		let nabmbar = make_io_space device.resources.(1)
		
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
		
		(*let ready_queue = Queue.create ()
		let free_queue  = Queue.create ()
		
		let isr () =
			if next_buffer (last_valid ()) <> current () then begin
				(* clear the interrupt, by writing '1' to bit 3 *)
				nabmbar.write16 R.status (nabmbar.read16 R.status lor 8);
				Mutex.lock m;
				Queue.push buffers.(last_valid ()) free_queue;
				Condition.broadcast cv;
				Mutex.unlock m;
			end
		
		let play () = ignore (Thread.create (fun () ->
			while true do
				Mutex.lock m;
				while Queue.is_empty ready_queue do
					Condition.wait cv m;
				done;
				let buffer = Queue.pop ready_queue in
				Mutex.unlock m;
				match buffer with
				| `Full buffer ->
					Mutex.lock m;
					while next_buffer (last_valid ()) = current () do
						Condition.wait cv m;
					done;
					Mutex.unlock m;
					let ix = next_buffer (last_valid ()) in
					bdl.{2*ix+1} <- Int32.logor 0x8000_0000l (Int32.of_int ((buffer_size * 2) lsr 1));
					nabmbar.write8 R.last_valid ix;
				| `Partial buffer ->
					Mutex.lock m;
					while next_buffer (last_valid ()) = current () do
						Condition.wait cv m;
					done;
					Mutex.unlock m;
					let ix = next_buffer (last_valid ()) in
					bdl.{2*ix+1} <- Int32.logor 0x8000_0000l (Int32.of_int ((Array1.dim buffer) lsr 1));
					nabmbar.write8 R.last_valid ix;
			done) () "ac97")
		
		let get_buffer () =
			Mutex.lock m;
			while Queue.is_empty free_queue do
				Condition.wait cv m;
			done;
			let b = Queue.pop free_queue in
			Mutex.unlock m;
			b
		
		let put_buffer buffer =
			Mutex.lock m;
			Queue.push buffer ready_queue;
			Condition.broadcast cv;
			Mutex.unlock m*)

		open BlockIO
		(* from old ICH0 sources... *)
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
					Debug.printf "no more audio\n";
				end else begin
					(* we can shuffle more data into the card *)
					Debug.printf "adding another buffer\n";
					Mutex.lock m;
					while next_buffer (last_valid ()) = current () do
						(* wait for a free buffer *)
						Debug.printf "buffers full\n";
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
	Printf.printf "ich0: on request line %02X\n" device.request_line;
	
	(*(* add all the buffers to the free queue *)
	for i = 0 to Array.length C.buffers - 1 do
		Queue.push C.buffers.(i) C.free_queue;
	done;*)
	(* register an interrupt handler *)
	Interrupts.create device.request_line C.isr;
	C.set_bit C.nabmbar R.control 4; (* interrupts for buffer completion *)
	Printf.printf "ich0: on request line %02X\n" device.request_line;
	
	(* start output *)
	C.nabmbar.write8 R.control (C.nabmbar.read8 R.control lor 1);
	
	(* register with the audio mixer *)
	let sample_rate = C.nambar.read16 R.sample_rate in
	Printf.printf "ich0: sample rate = %d\n" sample_rate;
	AudioMixer.register_device {
		format = 16, sample_rate, 2;
		output = C.output;
	}

let init () =
	DeviceManager.add_driver "intel ac'97" create 0x8086 0x2415; (* http://svn.berlios.de/wsvn/haiku/haiku/trunk/src/add-ons/kernel/drivers/audio/ac97/ichaudio/ichaudio.c *)
	DeviceManager.add_driver "intel ac'97" create 0x8086 0x293E
