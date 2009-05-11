
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
	mutable src_port : int;
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
	(* to wait for connection to be established *)
	m : Mutex.t;
	cv : Condition.t;
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
		(Bitstring.bitstring_of_string data)

let (++) = Int32.add (* so we have an infix operator for addition *)
let one = Int32.one

let empty = ""

let on_input cookie packet =
	let has_flag f = List.mem f packet.flags in
	begin match cookie.status.mode with
		| Syn_sent when has_flag Syn && has_flag Ack ->
			(* establishing connection *)
			if packet.ack_num <> cookie.status.s_next then begin
				Vt100.printf "tcp: ack# not equal next seq#, reset connection\n";
				(* should close the connection now *)
				NetworkStack.unbind_tcp cookie.src_port;
				send cookie Int32.zero Int32.zero [Reset] empty;
			end else begin
				Vt100.printf "tcp: connection established\n";
				cookie.status.mode <- Established;
				cookie.status.r_next <- packet.seq_num ++ one;
				(* signal cv to say connection established *)
				Mutex.lock cookie.m;
				Condition.signal cookie.cv;
				Mutex.unlock cookie.m;
				(* send ACK to complete handshake *)
				send cookie packet.ack_num cookie.status.r_next [Ack] empty
			end
		| Established ->
			if packet.seq_num <> cookie.status.r_next then begin
				Vt100.printf "tcp: seq# not equal r_next\n";
			end else begin
				(* we have some packet data *)
				let packet_data = Bitstring.string_of_bitstring packet.content in
				let len = String.length packet_data in
				(* update r_next to next sequence #, current + data length *)
				cookie.status.r_next <- cookie.status.r_next ++ (Int32.of_int len);
				if has_flag Finish then begin
					(* send FIN & ACK *)
					cookie.status.r_next <- cookie.status.r_next ++ one;
					cookie.status.mode <- Closing;
					send cookie cookie.status.s_next cookie.status.r_next [Ack;Finish] empty;
				end else begin
					(* send ACK *)
					send cookie cookie.status.s_next cookie.status.r_next [Ack] empty;
				end;
				if (len < cookie.status.window_size && len > 0) || has_flag Push then begin
					(* reassemble and push to application layer *)
					Vt100.printf "tcp: push data to app layer\n"
				end else if len = 0 then begin
					()
				end else begin
					(* add to packets to be reassembled later *)
					Vt100.printf "tcp: caching packet data, to be reassembled later\n"
				end
			end
		| Closing when has_flag Ack ->
			(* close the connection *)
			cookie.status.mode <- Closed;
			NetworkStack.unbind_tcp cookie.src_port;
			Vt100.printf "tcp: connection closed\n";
		| Closed ->
			Vt100.printf "tcp: received data on closed connection\n";
		| _ ->
			Vt100.printf "unhandled tcp state\n";
			NetworkStack.unbind_tcp cookie.src_port
	end

let rec do_output cookie app_data =
	match String.length app_data with
	| 0 ->
		(* no more data to send *)
		()
	| n when n > cookie.status.window_size ->
		(* send up to window_size bytes *)
		let data = String.sub app_data 0 cookie.status.window_size in
		let s_next = cookie.status.s_next in
		cookie.status.s_next <- cookie.status.s_next ++ (Int32.of_int cookie.status.window_size);
		send cookie s_next cookie.status.r_next [Ack] data;
		do_output cookie (String.sub app_data cookie.status.window_size (n - cookie.status.window_size))
	| n when n = cookie.status.window_size ->
		(* send the rest, which is exact fit *)
		let s_next = cookie.status.s_next in
		cookie.status.s_next <- cookie.status.s_next ++ (Int32.of_int n);
		send cookie s_next cookie.status.r_next [Ack;Push] app_data
	| n ->
		(* send the rest, less than window_size *)
		let s_next = cookie.status.s_next in
		cookie.status.s_next <- cookie.status.s_next ++ (Int32.of_int n);
		send cookie s_next cookie.status.r_next [Ack;Push] app_data

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
			do_output = begin fun _ -> failwith "tcp: not ready!" end; (* fixme! *)
			status = {
				s_next = seq ++ one;
				r_next = Int32.zero;
				window_size = 512;
				mode = Syn_sent;
			};
			m = Mutex.create ();
			cv = Condition.create ();
		}
	in
	
	t.on_input <- on_input t;
	
	(* bind to the network stack first *)
	NetworkStack.bind_tcp t.src_port t.on_input;
	
	(* send connection initiation packet *)
	(* flags = SYN, seq = x *)
	send t seq Int32.zero [Syn] empty;
	(* wait for connection to be established *)
	Mutex.lock t.m;
	while t.status.mode = Syn_sent do
		Condition.wait t.cv t.m;
	done;
	Mutex.unlock t.m;
	(* connection established *)
	t.do_output <- do_output t;
	(* return function so we can test sending something... *)
	t.do_output

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

	