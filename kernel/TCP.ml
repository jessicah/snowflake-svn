
(* TCP Module *)

open NetworkProtocolStack.TCP

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

type t = {
	(* port we've bound to *)
	src_port : int;
	(* port we've connected to *)
	dst_port : int;
	(* ip we've connected to *)
	dst_ip : NetworkProtocolStack.IPv4.addr;
	(* function called by the network stack when we've received a TCP packet
	   that matches the port we're bound to -- not port defined above *)
	mutable on_input : NetworkProtocolStack.TCP.t -> unit;
	(* function called (indirectly) by the application layer to send data to
	   the end-point. do_output invokes the network stack to send packet(s) *)
	mutable do_output : string -> unit;
	(* maintains the state for this TCP connection *)
	status : status;
}

(* gets a port to bind to; it's a nasty hack *)
let get_port =
	let next = ref 60000 in
	begin fun () -> decr next; !next end

(* utility function to send TCP packets *)
let send t seq ack flags data =
	NetworkStack.send_tcp
		t.src_port
		t.dst_port
		seq
		ack
		flags
		t.status.window_size
		t.dst_ip
		data

let (++) = Int32.add (* so we have an infix operator for addition *)
let one = Int32.one

let empty = Bitstring.empty_bitstring

let on_input cookie packet =
	let has_flag f = List.mem f packet.flags in
	begin match cookie.status.mode with
		| Syn_sent when has_flag Syn && has_flag Ack ->
			(* establishing connection *)
			if packet.ack_num <> cookie.status.s_next then begin
				Vt100.printf "tcp: ack# not equal next seq#\n";
				(* should close the connection now *)
				NetworkStack.unbind_tcp cookie.src_port;
			end else begin
				Vt100.printf "tcp: connection established\n";
				cookie.status.mode <- Established;
				cookie.status.r_next <- packet.seq_num ++ one;
				(* send ACK to complete handshake *)
				send cookie packet.ack_num cookie.status.r_next [Ack] empty
			end
		| Established ->
			if packet.seq_num <> cookie.status.r_next then begin
				Vt100.printf "tcp: seq# not equal r_next\n";
			end else begin
				(* we have some packet data *)
				let len = Bitstring.bitstring_length packet.content / 8 in
				(* update r_next to next sequence #, current + data length *)
				cookie.status.r_next <- cookie.status.r_next ++ (Int32.of_int len);
				(* send ACK *)
				send cookie cookie.status.s_next cookie.status.r_next [Ack] empty;
				if len < cookie.status.window_size || has_flag Push then begin
					(* reassemble and push to application layer *)
					Vt100.printf "tcp: have data to push to application\n"
				end else begin
					(* add to packets to be reassembled later *)
					Vt100.printf "tcp: caching packet data, to be reassembled later\n"
				end
			end
		| _ ->
			Vt100.printf "unhandled tcp state\n";
			NetworkStack.unbind_tcp cookie.src_port
	end

(* connect to an end-point *)
let connect ip port =
	(* our sending sequence seed *)
	let seq = Random.int32 0xBABE_l in
	(* establish our state *)
	let t = {
			src_port = get_port ();
			dst_port = port;
			dst_ip = ip;
			on_input = ignore; (* fixme! *)
			do_output = ignore; (* fixme! *)
			status = {
				s_next = seq ++ one;
				r_next = Int32.zero;
				window_size = 512;
				mode = Syn_sent;
			};
		}
	in
	
	t.on_input <- on_input t;
	
	(* bind to the network stack first *)
	NetworkStack.bind_tcp t.src_port t.on_input;
	
	(* send connection initiation packet *)
	(* flags = SYN, seq = x *)
	send t seq Int32.zero [Syn] empty
	(*
			
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
	send packet*)

(* this is close, but not quite what we want *)
type segment = (int * int * string) list

let reassemble segment =
	let rec check = function
		| (a,b,_) :: (d,e,f) :: xs when a + b = d -> check ((d,e,f) :: xs)
		| _ :: [] | [] -> true
		| _ -> false
	in
	let segment = List.sort (fun (a,_,_) (d,_,_) -> compare a d) segment in
	assert (check segment);
	String.concat "" (List.map (fun (_,_,c) -> c) segment)

	