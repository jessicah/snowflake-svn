
(* partition reading code... *)

let codes = [
		0x00, "Empty";
		0x05, "Extended partition";
		0x07, "NTFS";
		0x0B, "FAT32";
		0x0F, "Extended partition (LBA)";
		0x82, "Linux swap";
		0x83, "Linux";
	]

let code_to_string code =
	begin try
		List.assoc code codes
	with Not_found ->
		Printf.sprintf "Other: %02XH" code
	end

type partition = {
	code: int;
	start: int;
	length: int;
}

let read_bytes i n =
	let rec read acc = function
		| 0 -> List.rev acc
		| n -> read (IO.read_byte i :: acc) (n-1)
	in read [] n

(* f : offset -> num_sectors -> string[0 .. 512*num_sectors-1] *)
let partitions f =
	let rec read offset =
		(* partition table starts from the 446th byte of the first sector *)
		(* IO.input from a sector of 512 bytes *)
		let i = IO.input_string (f offset 1) in
		ignore (read_bytes i 446);
		let tables = List.map (fun _ -> IO.really_nread i 16) [1; 2; 3; 4] in
		let magic = read_bytes i 2 in
		if magic <> [0x55;0xAA] then begin
			if offset = 0 then begin
				let a = List.hd magic
				and b = List.hd (List.tl magic) in
				Vt100.printf "No partition table found. Magic = %02x%02x\r\n" a b;
			end;
			[]
		end else
			(* : partition list *)
			let partitions = List.map (fun string ->
				let i = IO.input_string string in
				ignore (read_bytes i 4);
				let code = IO.read_byte i in
				ignore (read_bytes i 3);
				let start = offset + (IO.read_i32 i) in
				let length = IO.read_i32 i in
				{ code = code; start = start; length = length }
			) tables in
			(* : partition list list *)
			let ext_tables = List.map (fun p ->
				if p.code = 0x05 || p.code = 0x0F
					then read p.start
					else []
			) partitions in
			partitions @ List.flatten ext_tables
	in
	read 0 (* start from the beginning, naturally *)

(* functions to read/write inside a partition -- basically does the offset stuff for you *)
let wrap_read f partition offset length =
	f (partition.start + offset) length

let wrap_write f partition offset length data =
	f (partition.start + offset) length data

(* e.g. to read from a partition, p, on ide disk: primary slave, would have like:
	let disk = IDE.get IDE.Primary IDE.slave in
	let my_read_funcion = wrap_read (IDE.read_disk disk) in
	let some_data = my_read_function some_offset some_number_of_sectors in
	(* process some_data *)
*)
