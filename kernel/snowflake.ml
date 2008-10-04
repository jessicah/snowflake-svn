
let echo_shell () =
	while true do
		Vt100.printf "%c" (Keyboard.get_char ())
	done

let (++) x f = f x

let print_ip () = function IPv4.Addr (x1,x2,x3,x4) ->
	Printf.sprintf "%d:%d:%d:%d" x1 x2 x3 x4

let print_dhcp packet =
	Vt100.printf "Transaction ID: %ld\r\n" packet.DHCP.transaction;
	Vt100.printf "Client IP: %a\r\n" print_ip packet.DHCP.client;
	Vt100.printf "Server IP: %a\r\n" print_ip packet.DHCP.server;
	Vt100.printf "Options: %d present\r\n" (List.length packet.DHCP.options);
	begin try 
		let z = List.find (fun o -> (fst o) = 0x35) packet.DHCP.options in
		Vt100.printf "Message: %d\r\n" (List.hd (snd z))
	with Not_found -> () end

let print_device dev =
	Vt100.printf "PCI: %04X:%04X\r\n" dev.PCI.vendor dev.PCI.device

open PCI

let rec (<->) a = function
	| b when a < b -> a :: (<->) (a + 1) b
	| b when a > b -> List.rev (b <-> a)
	| b -> [b]

module Foo = struct
open IO;;
open ExtList;;
open ExtString;;

let read_bytes i n =
	let rec read = function
	| 0 -> []
	| n -> read_byte i :: read (n-1) in
	List.rev (read n);;

let write_bytes o = List.iter (write_byte o);;

(* General Utilities *)

let rec fix_width list bytes = match list,bytes with
| _,0 -> []
| [],n -> 0::fix_width [] (n-1)
| x::xs,n -> x::fix_width xs (n-1);;

let rec overwrite offset sublist list = match (offset,sublist,list) with
| (_, [], list) -> list
| (0, a::ax, _::bx) -> a::overwrite offset ax bx
| (n, _,b::bx) when n > 0 -> b::overwrite (offset-1) sublist bx
| (_,_,_) -> failwith "Invalid argument to overwrite"

let checksum data =
	let data = List.map int_of_char data in
	let rec do_checksum = function
	| a::b::rest -> ((a lsl 8) lor b) + do_checksum rest
	| [a] -> a
	| [] -> 0
	in let result = (lnot (do_checksum data mod 0xFFFF)) land 0xFFFF in
	List.map char_of_int [result asr 8; result land 0xFF]

type token = Byte | Word | DWord | Bytes of int | Rest;;

(* token list -> int list -> char list *)
let compose_gen format data =
	let o = IO.output_enum () in
	let compose = function
	| Byte,[b] -> IO.write_byte o b
	| Word,[w]-> IO.BigEndian.write_ui16 o w
	| DWord,[d] -> IO.BigEndian.write_i32 o d
	| Bytes n,ls -> write_bytes o (fix_width ls n)
	| Rest,ls -> write_bytes o ls
	in List.iter compose (List.combine format data);
	List.of_enum (IO.close_out o)

(* token list -> char list -> int list *)
let decompose_gen format data =
	let i = IO.input_enum (List.enum data) in
	let rec decompose list = function
	| Byte    -> [IO.read_byte i]
	| Word    -> [IO.BigEndian.read_ui16 i]
	| DWord   -> [IO.BigEndian.read_i32 i]
	| Bytes 0 -> List.rev list
	| Bytes n -> decompose (IO.read_byte i :: list) (Bytes (n-1))
	| Rest    -> try decompose (IO.read_byte i :: list) Rest with IO.No_more_input -> List.rev list
	in List.map (decompose []) format;;

(* Internet Protocol *)

module IP = struct
	let format = [ Byte; Byte; Word; DWord; Byte; Byte; Word; Bytes 4; Bytes 4 ]
	
	let compose version tos _ ttl proto src_ip dst_ip data =
		let header = compose_gen format [[(version lsl 4) lor 5]; [tos];[String.length data + 20];[0];[ttl];[proto];[0];src_ip;dst_ip] in
		let checksum = checksum header in
		let packet = (overwrite 10 checksum header) @ (String.explode data) in
		String.implode packet
	let decompose packet = 
		let [hlen]::rest = decompose_gen [Byte;Rest] packet in
		let num_options = (hlen land 0xFF - 5) in
		let options = List.map (fun _ -> Bytes 4) (1 <-> num_options) in
		decompose_gen (format @ options @ [Rest]) packet
end;;

(* UDP *)

module UDP = struct
	let format = [ Word; Word; Word; Word; Rest ]
	
	let compose src_ip src_port dst_ip dst_port data =
		let packet = compose_gen format ([[src_port];[dst_port];[List.length data + 8];[0]] @ [data]) in
		let header = compose_gen [Bytes 4; Bytes 4; Word; Word] [src_ip;dst_ip;[List.length packet];[0x11]] in (* WTF length - 4?? *)
		let checksum = checksum (header@packet) in
		let packet = overwrite 6 checksum packet in
		String.implode packet
end;;

let make_ethernet addr proto dst packet =
		let o = output_string () in
		write_bytes o dst;
		write_bytes o addr;
		write_ui16 o proto;
		nwrite o packet;
		close_out o
	let make_ip addr dst packet =
		make_ethernet addr 0x0800 dst packet
end

module DHCPClient = struct
	module Udp = UDP
	open Foo
	
	type client = {
		send : string -> unit;
		recv : unit -> string;
		addr : int list
	}
	
	let parser =
		DHCP.packet ++ Udp.packet ++ IPv4.packet ++ Ethernet.packet
	
	let as_packet string =
		let array = Array.create (String.length string) 0 in
		for i = 0 to Array.length array - 1 do
			array.(i) <- Char.code string.[i];
		done;
		Packet.of_array array
	
	let create (r,s,a) = {
			send = s;
			recv = r;
			addr = a;
		}
	
	let make_packet addr ptype =
		let pad n = List.map (fun _ -> 0) (1 <-> n) in
		let dhcp_packet = 
			[1;1;6;0;0x11;0x22;0x33;0x44;0;0;0x80;0x00]
			@ pad 16
			@ addr
			@ pad 202
			@ [0x63;0x82;0x53;0x63]
			@ ptype
			@ [0x37;0x07;0x01;0x1c;0x02;0x03;0x0f;0x06;0x0c;0xff]
			@ (pad (300 - (248 + List.length ptype)))
		in
		make_ip addr [255;255;255;255;255;255] (IP.compose 0x4 0x00 0x001 0xff 0x11 [0;0;0;0] [255;255;255;255] (
			UDP.compose [0;0;0;0] 68 [255;255;255;255] 67 dhcp_packet
		))
	
	let register client =
		Vt100.printf "Sending DHCP Discover...\r\n";
		let p = make_packet client.addr [0x35;0x01;0x01] in
		client.send p;
		let p2 = as_packet p in
		print_dhcp (Parser.parse_packet parser (as_packet p)).Ethernet.content.IPv4.content.Udp.content;
		Vt100.printf "Awaiting DHCP Reply...\r\n";
		let reply = Parser.parse_packet parser (as_packet (client.recv ())) in
		Vt100.printf "Dumping reply packet...\r\n";
		print_dhcp reply.Ethernet.content.IPv4.content.Udp.content;
		Vt100.printf "Done!\r\n"
end

let () =
    Vga.init (); (* set up a pretty console font *)
	Keyboard.init (); (* set up the keyboard handler *)
	Vt100.printf "Hello, from ML :)\nUsing ocaml version: %s\n" Sys.ocaml_version;
	Asm.sti ();
	(*let dhcp_parser =
		DHCP.packet ++ UDP.packet ++ IPv4.packet ++ Ethernet.packet
	in
	let packet = Parser.parse_packet dhcp_parser Test.dhcp in
	print_dhcp packet.Ethernet.content.IPv4.content.UDP.content;*)
	let pci_devices = PCI.probe_bus () in
	List.iter print_device pci_devices;
	begin try
		let dev = List.find (fun d -> d.vendor = 0x10EC && d.device = 0x8139) pci_devices in
		let x = RealTek8139.create dev in
		Vt100.printf "Created realtek 8139 device\r\n";
		let client = DHCPClient.create x in
		DHCPClient.register client;
	with Not_found ->
		Vt100.printf "No realtek 8139 found\r\n";
	end;
	ignore (Thread.create echo_shell ()) (* start the echo shell *)
