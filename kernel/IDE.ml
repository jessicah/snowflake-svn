
(* start out small; only support primary master *)

exception Timeout

type t = int

type disk = { sink : KernelBuffer.sink; source : KernelBuffer.source }

module R = struct
	let data     = 0x00
	let error    = 0x01
	let features = 0x01
	let seccount = 0x02
	let lba_low  = 0x03
	let lba_mid  = 0x04
	let lba_high = 0x05
	let dev_head = 0x06
	let status   = 0x07
	let command  = 0x07
end

module C = struct
	let diagnostic = 0x90
	let read_sectors = 0x20
	let write_sectors = 0x30
	let identify = 0xEC
end

module S = struct
	let err  = 0x01
	let idx  = 0x02
	let corr = 0x04
	let drq  = 0x08
	let dsc  = 0x10
	let df   = 0x20
	let drdy = 0x40
	let bsy  = 0x80
end

let pri = 0x1F0

let rec poll ofs f lim =
	if not (f (Asm.in8 (pri+ofs))) && lim < 100_000 then begin
		(*Thread.yield ();*)
		Asm.out8 0x80 0x80;
		ignore (Asm.in8 0x80);
		poll ofs f (lim+1)
	end else if lim = 100_000 then begin
		raise Timeout
	end else begin (* preserve tail recursion *)
		()
	end

let poll ofs f = poll ofs f 0

let intnot = function
	| 0 -> 1
	| _ -> 0

let read offset =
	poll R.status (fun i -> i land S.bsy = 0);
	Asm.in8 (pri+offset)

let write offset data =
	poll R.status (fun i -> i land (S.bsy lor S.drq) = 0);
	Asm.out8 (pri+offset) data

let swab s =
	for i = 0 to String.length s / 2 - 1 do
		let t = s.[i * 2] in
		s.[i * 2] <- s.[i * 2 + 1];
		s.[i * 2 + 1] <- t;
	done;
	s

(* disk is disk_to_id, 0, 1, 2, 3 *)
let read_disk disk sector length = (* disk = 0x00/0x10 master/slave *)
	(*poll R.status (fun i -> i land S.bsy = 0);*)
	write R.seccount length;
	write R.lba_low (sector land 0xFF);
	write R.lba_mid ((sector lsr 8) land 0xFF);
	write R.lba_high ((sector lsr 16) land 0xFF);
	write R.dev_head (0xE0 lor disk lor ((sector lsr 24) land 0x0F));
	write R.command C.read_sectors;
	poll R.status (fun i -> i land S.bsy = 0 && i land S.drdy <> 0);
	let data = String.create (512 * length) in
	for i = 0 to length - 1 do
		String.blit
			(Asm.in16s (pri+R.data) 256) 0
			data (i * 512)
			512;
	done;
	data
	
let present_disks= [| false; false; false; false |]

type controller = Primary | Secondary
type device = Master | Slave

let disk_to_id controller device = match controller, device with
	| Primary, Master -> 0
	| Primary, Slave -> 1
	| Secondary, Master -> 2
	| Secondary, Slave -> 3
	
type device_t = {
	read : int -> int -> string;
	write : int -> int -> string -> unit;
}

(* KernelBuffer type stuff *)

open KernelBuffer

let mk_source disk_id =
	let rec src = {
		position = 0;
		length = max_int;
		units = 512;
		offset = 0;
		fill = begin fun buf ofs len ->
				let s = read_disk disk_id src.position ((len + src.units - 1) / src.units) in
				(* blit from string into bigarray buffer *)
				BlockIO.blit_from_string s buf;
				len (* how much we wrote *)
			end;
		}
	in src

let kb_disks =
	let invalid = { sink = null_sink; source = null_source } in
	[| invalid; invalid; invalid; invalid |]

let init () =
	(* look for an ata controller *)
	Asm.out8 (pri+R.seccount) 0xEC;
	if Asm.in8 (pri+R.seccount) <> 0xEC then begin
		Vt100.printf "ide: no controller found\n"
	end else begin
	
	(* get status of disks *)
	write R.dev_head 0x00;
	write R.command C.diagnostic;
	let masterStatus = read R.error in
	write R.dev_head 0x10;
	write R.command C.diagnostic;
	let slaveStatus = read R.error in
	
	(* master present if masterStatus = 1 *)
	(* slave present if slaveStatus < 0x80 *)
		if masterStatus land 0x01 = 0x01 then begin
			Vt100.printf "ide: found primary master\n";
			present_disks.(0) <- true;
		end else begin
			Vt100.printf "ide: can't find primary master\n";
		end;
		if slaveStatus land 0x01 = 0x01 then begin
			Vt100.printf "ide: found primary slave\n";
			present_disks.(1) <- true;
		end else begin
			Vt100.printf "ide: can't find primary slave\n";
		end;
	end;
	Array.iteri (fun i d ->
		if present_disks.(i) then
			kb_disks.(i) <- { kb_disks.(i) with source = mk_source i }) kb_disks
			
let disks =
	let invalid = {
		read = (fun _ _ -> raise Not_found);
		write = (fun _ _ _ -> raise Not_found);
	} in
	[| invalid; invalid; invalid; invalid |]

let init () =
	(* look for an ata controller *)
	Asm.out8 (pri+R.seccount) 0xEC;
	if Asm.in8 (pri+R.seccount) <> 0xEC then begin
		Vt100.printf "ide: no controller found\n"
	end else begin
	
	(* get status of disks *)
	write R.dev_head 0x00;
	write R.command C.diagnostic;
	let masterStatus = read R.error in
	write R.dev_head 0x10;
	write R.command C.diagnostic;
	let slaveStatus = read R.error in
	
	(* master present if masterStatus = 1 *)
	(* slave present if slaveStatus < 0x80 *)
		if masterStatus land 0x01 = 0x01 then begin
			Vt100.printf "ide: found primary master\n";
			present_disks.(0) <- true;
		end else begin
			Vt100.printf "ide: can't find primary master\n";
		end;
		if slaveStatus land 0x01 = 0x01 then begin
			Vt100.printf "ide: found primary slave\n";
			present_disks.(1) <- true;
		end else begin
			Vt100.printf "ide: can't find primary slave\n";
		end;
	end;
	Array.iteri (fun i d ->
		if present_disks.(i) then
			disks.(i) <- { disks.(i) with read = read_disk i }) disks;
	init ()

let get c d =
	let id = disk_to_id c d in
	if present_disks.(id) then
		id
	else
		raise Not_found

open PCI

let create dev =
	begin try
		(*let dev = PCI.probe dev.b dev.d 1 in
	Vt100.printf "probing device %04x:%04x\r\n" dev.vendor dev.device;
	Vt100.printf "programming interface: %x\r\n"
		(PCI.read8 dev.id 9);
	(*(* write 1f0 into the bmiba *)
	PCI.write32 dev.id 0x20
		(Int32.of_int ((0x1f0 lsl 4) lor 1));*)
	(*(* write bit to set I/O space enable to command register *)
	PCI.write16 dev.id 0x04 1;*)
	(* try init() again *)
	Vt100.printf "pci ide: init...\r\n";*)
	init ();
	with Not_found -> Vt100.printf "no sub device" end
	
let init2 () =
	DeviceManager.add_driver "Generic IDE Controller: Intel PIIX4" create 0x8086 0x7111;
	DeviceManager.add_driver "Generic IDE Controller: Intel PIIX3" create 0x8086 0x7000;
	DeviceManager.add_driver "Generic IDE Controller: Intel ICH0" create 0x8086 0x2411;
	DeviceManager.add_driver "Generic IDE Controller: Intel ICH0" create 0x8086 0x2421
