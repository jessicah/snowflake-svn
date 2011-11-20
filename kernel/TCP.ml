
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
	mutable on_input : PacketLists.TCP.packet -> int -> unit;
	(* function called (indirectly) by the application layer to send data to
	   the end-point. do_output invokes the network stack to send packet(s) *)
	mutable do_output : string -> unit;
	(* maintains the state for this TCP connection *)
	status : status;
	(* to wait for connection to be established *)
	m : Mutex.t;
	cv : Condition.t;
	(* buffer for received (reassembled) packets *)
	rb : RingBuffer.t;
	(* receive packet queue *)
	rxq : (PacketLists.TCP.packet * int) MVar.t;
}

type tcp_readline = {
	tcp : t;
	send : string -> unit;
	recv : unit -> string;
}

let used_ports = ref []

let rec get_port () =
	let next = Random.int 60000 + 1024 in
	(* make sure it's not used, else try again *)
	if List.mem next !used_ports then get_port ()
	else begin
		(* add it to the used ports list and return it *)
		used_ports := next :: !used_ports;
		next
	end

(* utility function to send TCP packets *)
let send t seq ack flags data =
	let start = Asm.rdtsc () in
	NetworkStack.send_tcp
		t.src_port
		t.dst_port
		seq
		ack
		flags
		t.status.window_size
		t.dst_ip
		(Bitstring.bitstring_of_string data);
	Debug.log "tcp-send" start (Asm.rdtsc())

let (++) = Int32.add (* so we have an infix operator for addition *)
let one = Int32.one

let empty = ""

module L = PacketLists

(* modified to be a thread instead *)
let input_thread cookie =
		while cookie.status.mode <> Closed do
	let (packet, packet_length) = MVar.get cookie.rxq in
	let start = Asm.rdtsc () in
	let has_flag f = List.mem f (to_flags (packet.L.TCP.flags)) in
	begin match cookie.status.mode with
		| Syn_sent when (*has_flag Syn &&*) has_flag Ack ->
			(* establishing connection *)
			if packet.L.TCP.ack <> cookie.status.s_next then begin
				Printf.printf "tcp: ack# (%lx) not equal next seq# (%lx), reset connection\n"
					packet.L.TCP.ack cookie.status.s_next;
				(* should close the connection now *)
				NetworkStack.unbind_tcp cookie.src_port;
				RingBuffer.close cookie.rb;
				send cookie Int32.zero Int32.zero [Reset] empty;
			end else begin
				(*Printf.printf "tcp: connection established\n";*)
				cookie.status.mode <- Established;
				cookie.status.r_next <- packet.L.TCP.seq ++ one;
				(* signal cv to say connection established *)
				Mutex.lock cookie.m;
				Condition.signal cookie.cv;
				Mutex.unlock cookie.m;
				(* send ACK to complete handshake *)
				send cookie packet.L.TCP.ack cookie.status.r_next [Ack] empty
			end
		| Established ->
			if packet.L.TCP.seq <> cookie.status.r_next then begin
				(*Printf.printf "tcp: seq# not equal r_next\n";*)
				(* ignore it *)
				()
			end else begin
				(* we have some packet data *)
				let intlist = packet.L.TCP.content in
				let packet_data =
					String.sub (ExtString.String.implode (List.map char_of_int intlist)) 0 (packet_length - 20)
				in
				let len = String.length packet_data in
				(* update r_next to next sequence #, current + data length *)
				cookie.status.r_next <- cookie.status.r_next ++ (Int32.of_int len);
				if has_flag Finish then begin
					(* send FIN & ACK *)
					cookie.status.r_next <- cookie.status.r_next ++ one;
					cookie.status.mode <- Closing;
					send cookie cookie.status.s_next cookie.status.r_next [Ack;Finish] empty;
				end else begin
					(* send ACK if we received data; don't need to ACK ACKs *)
					if packet.L.TCP.window > 0 && len > 0 then begin
						send cookie cookie.status.s_next cookie.status.r_next [Ack] empty;
					end
				end;
				if (*(len < cookie.status.window_size && len > 0) ||*) has_flag Push then begin
					(* reassemble and push to application layer *)
					RingBuffer.write cookie.rb packet_data;
				end else if len = 0 then begin
					if cookie.status.mode = Closing then begin
						cookie.status.mode <- Closed;
						RingBuffer.close cookie.rb;
						NetworkStack.unbind_tcp cookie.src_port;
					end;
				end else begin
					(* add to packets to be reassembled later *)
					RingBuffer.write cookie.rb packet_data;
				end
			end
		| Closing when has_flag Finish ->
			(* ack and close the connection *)
			cookie.status.mode <- Closed;
			RingBuffer.close cookie.rb;
			NetworkStack.unbind_tcp cookie.src_port;
			send cookie cookie.status.s_next cookie.status.r_next [Ack] empty;
			Printf.printf "tcp: closed connection\n";
		| Closing when has_flag Ack ->
			(* close the connection *)
			cookie.status.mode <- Closed;
			RingBuffer.close cookie.rb;
			NetworkStack.unbind_tcp cookie.src_port;
			(*send cookie Int32.zero Int32.zero [Reset] empty;*)
			Printf.printf "tcp: connection closed\n";
		| Closed ->
			Printf.printf "tcp: received data on closed connection\n";
		| _ ->
			Printf.printf "unhandled tcp state\n";
			RingBuffer.close cookie.rb;
			send cookie Int32.zero Int32.zero [Reset] empty;
			NetworkStack.unbind_tcp cookie.src_port
	end;
	Debug.log "tcp-recv" start (Asm.rdtsc());
		done (* thread terminates when mode = Closed *)

let on_input cookie packet packet_length =
	(* this should queue the packet and let it know there something ready for it *)
	MVar.put (packet, packet_length) cookie.rxq

let close tcp =
	tcp.tcp.status.mode <- Closing;
	send tcp.tcp tcp.tcp.status.s_next tcp.tcp.status.r_next [Finish] empty

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
	let seq = Random.int32 (Int32.max_int) in
	(* establish our state *)
	let t = {
			src_port = get_port ();
			dst_port = port;
			dst_ip = ip;
			on_input = begin fun _ _ -> () end; (* fixme! *)
			do_output = begin fun _ -> failwith "tcp: not ready!" end; (* fixme! *)
			status = {
				s_next = seq ++ one;
				r_next = Int32.zero;
				window_size = 64240; (* try a larger window size *)
				mode = Syn_sent;
			};
			m = Mutex.create ();
			cv = Condition.create ();
			rb = RingBuffer.create ();
			rxq = MVar.create ();
		}
	in
	
	t.on_input <- on_input t;
	ignore (Thread.create input_thread t "a tcp receive thread");
	
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
	t

let open_channel_io ip port =
	let t = connect ip port in
	t.do_output, RingBuffer.mk_input t.rb

let open_channel ip port =
	let t = connect ip port in
	(*let pos = ref 0 in
	let input = IO.from_in_channel(object(self)
			method input buf ofs len =
				if Queue.is_empty t.queue then begin
					if t.status.mode <> Established then begin
						(*Printf.printf "no more data";*)
						0
					end else begin
						pos := 0;
						Mutex.lock t.m;
						while Queue.is_empty t.queue do
							Condition.wait t.cv t.m;
						done;
						Mutex.unlock t.m;
						self#input buf ofs len
					end
				end else begin
					if !pos = String.length (Queue.peek t.queue) then begin
						ignore (Queue.pop t.queue);
						self#input buf ofs len
					end else begin
						let s = Queue.peek t.queue in
						let l = String.length s in
						match !pos, len with
						| 0, _ when len > l ->
							(*Printf.printf " %d" l;*)
							String.unsafe_blit s 0 buf ofs l;
							pos := l;
							l
						| 0, _ ->
							(*Printf.printf " %d" len;*)
							String.unsafe_blit s 0 buf ofs len;
							pos := len;
							len
						| _ ->
							let ll = min len (l - !pos) in
							(*Printf.printf " %d" ll;*)
							String.unsafe_blit s !pos buf ofs ll;
							pos := !pos + ll;
							ll
					end
				end
			method close_in () = ()
		end)*)
	(*let rec enum () =
		if Queue.is_empty t.queue && t.status.mode <> Established then
			raise Enum.No_more_elements;
		if Queue.is_empty t.queue then begin
			pos := 0;
			while Queue.is_empty t.queue do
				Thread.yield ();
			done;
		end;
		if !pos = String.length (Queue.peek t.queue) then begin
			ignore (Queue.pop t.queue);
			enum ()
		end else begin
			let ch = (Queue.peek t.queue).[!pos] in
			incr pos;
			ch
		end*)
	(*t.do_output, RingBuffer.mk_input t.rb*)
	t.do_output, RingBuffer.read_line t.rb

let open_readline ip port =
	let t = connect ip port in
	{ tcp = t; send = t.do_output; recv = RingBuffer.read_line t.rb }

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

let init () = ()
