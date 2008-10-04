
open Parser

type addr =
	Addr of int * int * int * int * int * int

let make x1 x2 x3 x4 x5 x6 =
	Addr (x1, x2, x3, x4, x5, x6)

let broadcast =
	make 0xFF 0xFF 0xFF 0xFF 0xFF 0xFF

let addr =
	make >>= word8 >> word8 >> word8
		>> word8 >> word8 >> word8

type 'a packet = {
		dst : addr;
		src : addr;
		packType : packetType;
		content : 'a;
	}
and packetType
	= Ethernet of int
	| IPv4
	| IPv6
	| ARP
	| Unknown of int

let to_type = function
	| x when x < 0x600 -> Ethernet x
	| 0x0800 -> IPv4
	| 0x86DD -> IPv6
	| 0x0806 -> ARP
	| x -> Unknown x

let packetType =
	to_type >>= word16

let packet content =
	(fun dst src packType content -> {
			dst = dst;
			src = src;
			packType = packType;
			content = content;
		})
	>>= addr >> addr >> packetType >> content
