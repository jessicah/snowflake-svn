
open Packet


	let buf = [|
		(* ethernet *)
		0x00; 0x11; 0x22; 0x33; 0x44; 0x55; (* src mac *)
		0x00; 0xca; 0xfe; 0xba; 0xbe; 0x00; (* dst mac *)
		0x08; 0x06; (* protocol *)
		(* ipv4 *)
		0x45; (* version + header len *)
		0x00; (* type of service *)
		00; 20; (* total length *)
		00; 00; (* identifier *)
		0x00; 0x00; (* flags + frag offset *)
		0x10; (* time to live *)
		0x11; (* protocol = UDP *)
		0xde; 0xad; (* checksum *)
		10; 10; 10; 10; (* src *)
		10; 10; 10; 12; (* dst *)
		(* no options *)
		(* udp *)
		00; 67; (* src port *)
		00; 68; (* dst port *)
		0x00; 0xFF; (* length *)
		0xde; 0xad; (* checksum *)
		(* content *)
		(* dhcp *)
		1; 1; 6; 0;
		0x12; 0x34; 0x56; 0x78; (* transaction id *)
		0x00; 0x00; 0x00; 0x00;
		0x00; 0x00; 0x00; 0x00;
		10; 10; 10; 12; (* client *)
		10; 10; 10; 254; (* server *)
		1;4;5;6;3;2;5;3;1;2;4;5; (* blah *)
		0x63; 0x82; 0x53; 0x63; (* magic *)
		0x35; 0x01; 0x05; (* acknowledge *)
		0x06; 0x04; 10; 10; 10; 251; (* dns *)
		0x03; 0x04; 10; 10; 10; 1; (* gateway *)
		0x01; 0x04; 255; 0; 0; 0; (* subnet *)
		0xFF; (* end of options *)
	|]

let dhcp = {
		buffer = buf;
		from = 0;
		len = Array.length buf;
	}
	