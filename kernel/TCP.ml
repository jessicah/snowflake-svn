
(* TCP Module *)

type mode
	= Listen
	| Syn_sent
	| Syn_recv
	| Established
	| Fin_wait_1
	| Fin_wait_2
	| Close_wait
	| Closing
	| Last_ack
	| Time_wait
	| Closed

type status = {
	mutable s_next : int32;
	mutable r_next : int32;
	mutable window_size : int;
	mutable mode : mode;
}

let get_port =
	let next = ref 60000 in
	begin fun () -> decr next; !next end

let connect ip port =
	(* our sending sequence seed *)
	let x = Random.int32 0xBABE_l in
	(* create a packet to the specified endpoint *)
	let packet = create ip port in
	(* set packet and status values *)
	status.s_next <- x + 1;
	packet.seq <- x;
	packet.flags <- [SYN];
	(* send the packet *)
	send packet;
	(* receive the packet -- in-place update *)
	recv packet;
	assert (packet.flags = [SYN;ACK]);
	assert (packet.ack = status.s_next);
	(* set packet and status values *)
	status.r_next <- packet.seq + 1;
	packet.seq <- status.r_next;
	packet.ack <- status.s_next;
	packet.flags <- [ACK];
	(* send the packet, establishing the connection *)
	send packet

type segment = (int * int * string) list

let reassemble segment =
	let rec check = function
		| (a,b,_) :: (d,e,f) :: xs when a + b = d -> check ((d,e,f) :: xs)
		| _ :: [] | [] -> true
		| _ -> false
	in
	let segment = List.sort (fun (a,_,_) (d,_,_) -> compare s1 s2) segment in
	assert (check segment);
	String.concat "" (List.map (fun (_,_,c) -> c) segment)

let read () =
	let packet = create ip port in
	(* receive the packet -- in-place update *)
	recv packet;
	(* update segment list *)
	segment <- packet.seq, len packet.data, packet data :: segment;
	(* send ack *)
	packet.ack <- packet.seq + len packet.data;
	packet.flags <- [ACK];
	send packet;
	(* if data has been pushed, return the reassembled segment *)
	if packet.flags & PSH then
		reassemble segment
	