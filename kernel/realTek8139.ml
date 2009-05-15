
(* RTL8139 Driver *)

open NetworkStack
open PCI
open Bigarray

module Registers = struct
	let idr0 = 0x0
	let command = 0x37
	let txconfig = 0x40
	let rxconfig = 0x44
	let config1 = 0x52
	let bmcr = 0x62
	let bmsr = 0x64
	let rbstart = 0x30
	let imr = 0x3C
	let isr = 0x3E
	let _9346cr = 0x50
	let config4 = 0x5A
	let tsd0 = 0x10
	let tsd1 = 0x14
	let mulint = 0x5C
	let tsd2 = 0x18
	let tsd3 = 0x1C
	let esrs = 0x36
	let tsad0 = 0x20
	let tsad1 = 0x24
	let tsad2 = 0x28
	let tsad3 = 0x2C
	let tsad = 0x60
	let capr = 0x38
	let cbr = 0x3A
	let mpc = 0x4C
	let mar0 = 0x08
end

module CommandActions = struct
	let reset = 0x10
	let enablereceive = 0x08
	let enabletransmit = 0x04
	let bufe = 0x01
end

module TransmitterActions = struct
	let mxdma_2 = 0x400
	let mxdma_1 = 0x200
	let mxdma_0 = 0x100
	let ifg_1 = 0x2000000
	let ifg_0 = 0x1000000
end

module ReceiverActions = struct
	let rxfth2 = 0x8000
	let rxfth1 = 0x4000
	let rxfth0 = 0x2000
	let rblen_1 = 0x1000
	let rblen_0 = 0x800
	let mxdma_2 = 0x400
	let mxdma_1 = 0x200
	let mxdma_0 = 0x100
	let wrap = 0x100
	let ab = 0x8
	let am = 0x4
	let apm = 0x2
end

module TransmitDescription = struct
	let own = 0x2000
	let tun = 0x4000
	let tok = 0x8000
	let owc = 0x20000000l
	let tabt = 0x40000000l
	let crs = 0x80000000l
end

module InterruptStatusBits = struct
	let receiveok = 0x01
	let receiveerror = 0x02
	let transmitok = 0x04
	let transmiterror = 0x08
	let receiveoverflow = 0x10
	let receiveunderflow = 0x20
	let receivefifooverrun = 0x40
	let timeout = 0x4000
	let systemerror = 0x8000
end

module BMCRCommands = struct
	let ane = 0x2000
	let duplex_mode = 0x100
	let restart_auto_negotiation = 0x400
end

type supported_device = {
	vendor_id: int;
	device_id: int;
	name: string;
}

let supported_devices =
	let make v d n = { vendor_id = v; device_id = d; name = n; } in
	[
		make 0x10EC 0x8139 "RealTek RTL8139 Fast Ethernet";
		make 0x10EC 0x8129 "RealTek RTL8129 Fast Ethernet";
		make 0x10EC 0x8138 "RealTek RTL8139B PCI/CardBus";
		make 0x1113 0x1211 "SMC1211TX EZCard 10/100 or Accton MPX5030 (Realtek RTL8139)";
		make 0x1186 0x1300 "D-Link DFE-538TX (RealTek RTL8139)";
		make 0x1186 0x1301 "D-LinLink DFE-530TX+ (RealTek RTL8139C)";
		make 0x1186 0x1340 "D-Link DFE-690TXD CardBus (RealTek RTL8139C)";
		make 0x018A 0x0106 "LevelOne FPC-0106Tx (RealTek RTL8139)";
		make 0x021B 0x8139 "Compaq HNE-300 (RealTek RTL8139c)";
		make 0x13D1 0xAB06 "Edimax EP-4103DL CardBus (RealTek RTL8139c)";
		make 0x02AC 0x1012 "Siemens 1012v2 CardBus (RealTek RTL8139c)";
		make 0x1432 0x9130 "Siemens 1020 PCI NIC (RealTek RTL8139c)";
	]

exception Break
exception Restart

let create pcii =
	(* set up the in/out functions *)
	let reg_base = match pcii.resources.(0) with
	| IO x -> x
	| _ -> failwith "rtl8139: can't get IO port"
	in
	let out8  offset value = Asm.out8  (reg_base + offset) value
	and out16 offset value = Asm.out16 (reg_base + offset) value
	and out32 offset value = Asm.out32 (reg_base + offset) value
	and in8  offset = Asm.in8  (reg_base + offset)
	and in16 offset = Asm.in16 (reg_base + offset)
	and in32 offset = Asm.in32 (reg_base + offset) in
	
	let m = Mutex.create () in
	let cv = Condition.create () in
	
	let module RTL8139 = struct
		type t =
		{
			pcii: device;
			reg_base: int;
			
			receivebuffer: (int, int8_unsigned_elt, c_layout) Array1.t;
			mutable receivebufferoffset: int;
			
			mutable writes: int;
			transmitbuffer: (int, int8_unsigned_elt, c_layout) Array1.t array;
			mutable transmitbusy: bool array;
			mutable queued_packets: int;
			mutable finished_packets: int;
			
			mutable multiset: int;
			address: int list;
		}
		
		let reset properties =
			Vt100.printf "rtl: resetting device\n";
			out8 Registers.command CommandActions.reset;
			out16 Registers.imr 0x00;
			out16 Registers.cbr 0x00;
			out16 Registers.capr (0 - 16);
			properties.receivebufferoffset <- 0;
			out16 Registers.imr (InterruptStatusBits.receiveok lor InterruptStatusBits.receiveerror lor InterruptStatusBits.transmitok lor InterruptStatusBits.transmiterror lor InterruptStatusBits.receiveoverflow lor InterruptStatusBits.receiveunderflow lor InterruptStatusBits.receivefifooverrun lor InterruptStatusBits.timeout lor InterruptStatusBits.systemerror);
			out8 Registers.command (CommandActions.enablereceive lor CommandActions.enabletransmit)
		
		let init () =
			out8 Registers.command CommandActions.reset;
			let temp = ref 10000 in
			while in8 Registers.command land CommandActions.reset <> 0 && !temp > 0 do
				decr temp;
			done;
			if !temp = 0 then failwith "RTL8139: Reset failed";
			out16 Registers.imr 0;
			out16 Registers.cbr 0;
			out16 Registers.capr (0 - 16);
			List.iter (fun (r,v) -> out8 r v) [
				Registers._9346cr, 0xC0;
				Registers.command, (CommandActions.enablereceive lor CommandActions.enabletransmit);
				Registers.config1, 0x00;
				Registers.config1, ((in8 Registers.config1 land (lnot 0x30)) lor 0x20);
				Registers.config4, (in8 Registers.config4 lor 0x80);
				Registers._9346cr, 0x00;
				Registers.mpc, 0x0000;
			];
			out32 Registers.txconfig (Int32.of_int (TransmitterActions.ifg_1 lor TransmitterActions.ifg_0 lor TransmitterActions.mxdma_1));
			out32 Registers.rxconfig (Int32.of_int (ReceiverActions.rblen_1 lor ReceiverActions.rblen_0 lor ReceiverActions.wrap lor ReceiverActions.mxdma_2 lor ReceiverActions.mxdma_1 lor ReceiverActions.apm lor ReceiverActions.ab));
			(* allocate buffers *)
			let receivebuffer = Array1.create Bigarray.int8_unsigned Bigarray.c_layout (1024 * 64 + 16) in
			let transmitbuffer1 = Array1.create Bigarray.int8_unsigned Bigarray.c_layout 2048 in
			let transmitbuffer2 = Array1.create Bigarray.int8_unsigned Bigarray.c_layout 2048 in
			let transmitbuffer3 = Array1.create Bigarray.int8_unsigned Bigarray.c_layout 2048 in
			let transmitbuffer4 = Array1.create Bigarray.int8_unsigned Bigarray.c_layout 2048 in
			out32 Registers.rbstart (Asm.address receivebuffer);
			out16 Registers.mulint 0x0000;
			out32 Registers.tsad0 (Asm.address transmitbuffer1);
			out32 Registers.tsad1 (Asm.address transmitbuffer2);
			out32 Registers.tsad2 (Asm.address transmitbuffer3);
			out32 Registers.tsad3 (Asm.address transmitbuffer4);
			let mac =
				List.map (fun i -> in8 (Registers.idr0 + i)) [0; 1; 2; 3; 4; 5]
			in
			List.iter (fun i -> Vt100.printf "%02x " i) mac;
			Vt100.printf "\r\n";
			out32 Registers.rxconfig (Int32.logor (in32 Registers.rxconfig) (Int32.of_int (ReceiverActions.apm lor ReceiverActions.ab)));
			out32 Registers.mar0 0x00l;
			out32 (Registers.mar0 + 4) 0x00l;
			let properties = {
				pcii = pcii;
				reg_base = reg_base;
				
				receivebuffer = receivebuffer;
				receivebufferoffset = 0;
				
				writes = 0;
				transmitbuffer = [| transmitbuffer1; transmitbuffer2; transmitbuffer3; transmitbuffer4; |];
				transmitbusy = [| false; false; false; false; |];
				queued_packets = 0;
				finished_packets = 0;
				
				multiset = 0;
				address = mac;
			} in
			out16 Registers.imr (InterruptStatusBits.receiveok lor InterruptStatusBits.receiveerror lor InterruptStatusBits.transmitok lor InterruptStatusBits.transmiterror lor InterruptStatusBits.receiveoverflow lor InterruptStatusBits.receiveunderflow lor InterruptStatusBits.receivefifooverrun lor InterruptStatusBits.timeout lor InterruptStatusBits.systemerror);
			out8 Registers._9346cr 0x00;
			out8 Registers.command (CommandActions.enablereceive lor CommandActions.enabletransmit);
			if in8 Registers.command land CommandActions.enablereceive = 0 then
				failwith "Receive not enabled";
			if in8 Registers.command land CommandActions.enabletransmit = 0 then
				failwith "Transmit not enabled";
			ignore (in16 Registers.bmsr);
			ignore (in8 Registers.esrs);
			properties
		
		let send properties packet =
			(*Mutex.lock m;*)
			while properties.writes = 4 do
				(*Condition.wait cv m;*)
				(* yield, because this should be fast *)
				Thread.yield ();
			done;
			(*Mutex.unlock m;*)
			try
				if String.length packet > 1792 then raise Break;
				let transmitid = properties.queued_packets mod 4 in
				(*Mutex.lock m;*)
				while properties.transmitbusy.(transmitid) do
					(*Condition.wait cv m;*)
					Thread.yield (); (* this should be fast *)
				done;
				(*Mutex.unlock m;*)
				properties.writes <- properties.writes + 1;
				properties.transmitbusy.(transmitid) <- true;
				(* this is a very inefficient loop; we really want bigarrays throughout, not strings... *)
				Array1.blit_from_string packet properties.transmitbuffer.(transmitid);
				let transmitdescription = (max (String.length packet) 60) lor 0x80000 in
				out32 (Registers.tsd0 + (4 * transmitid)) (Int32.logand (Int32.of_int transmitdescription) (Int32.lognot (Int32.of_int TransmitDescription.own)));
				properties.queued_packets <- properties.queued_packets + 1;
			with Break -> Vt100.printf "rtl.send error!\r\n"
		
		let rec read properties rx_buffer =
			try
				if in8 Registers.command land CommandActions.bufe <> 0 then
					raise Break;
				let packet_header = begin try
						if properties.receivebufferoffset = 65552 then properties.receivebufferoffset <- 0;
						Array1.sub properties.receivebuffer properties.receivebufferoffset 4
					with _ ->
						(* it appears the receiverbufferoffset is 8 bytes larger than the buffer size *)
						(* buffer size is 65552 = 65536 (0x10000) + 16 *)
						Vt100.printf "rtl: unable to extract packet header (%d, %d)\n" properties.receivebufferoffset
							(Array1.dim properties.receivebuffer);
						failwith "rtl: internal driver error";
					end
				in
				(* packet_header: uint16 bits, uint16 length, uint8 data[1] *)
				let length = (packet_header.{3} lsl 8) lor packet_header.{2}
				and bits = (packet_header.{1} lsl 8) lor packet_header.{0} in
				(*if length = 0xFFF0 then raise Restart;*)
				(* I don't understand why it resets the card if length > 1518... *)
				if bits land 0x1 = 0 (*|| length > 1518*) then (reset properties; raise Restart);
				(* hopefully a more efficient, and possibly correct, copying algo *)
				let packet = Array1.create int8_unsigned c_layout (length - 4) in
				begin try
					if properties.receivebufferoffset + (length - 4) > 65536 then begin
						let len = 65552 - (properties.receivebufferoffset + 4) in
						Array1.blit
							(Array1.sub properties.receivebuffer (properties.receivebufferoffset + 4) len)
							(Array1.sub packet 0 len);
						Array1.blit
							(Array1.sub properties.receivebuffer 0 (length - 4 - len))
							(Array1.sub packet len (length - 4 - len));
					end else begin
						Array1.blit
							(Array1.sub properties.receivebuffer (properties.receivebufferoffset + 4) (length - 4))
							packet;
					end
				with Invalid_argument _ ->
					if properties.receivebufferoffset + (length - 4) > 65536 then begin
						Vt100.printf "rtl: get packet fail (wrapping): %d (%d) + %d (%d) = %d bytes (%d expected)\n"
							(properties.receivebufferoffset + 4) (65552 - (properties.receivebufferoffset + 4))
							0 (length - 4 - (65552 - (properties.receivebufferoffset + 4)))
							((65552 - properties.receivebufferoffset + 4) + ((length - 4 - (65552 - (properties.receivebufferoffset + 4)))))
							(length - 4)
					end else begin
						Vt100.printf "rtl: get packet fail: %d %d bytes\n" (properties.receivebufferoffset + 4) (length - 4)
					end;
					failwith "rtl: packet extraction logic fail"
				end;
				(* the land (lnot 3) makes it a multiple of four... the adding of 3 appears to be due to the bitwise ops *)
				(* it doesn't seem to account for wrap-around... *)
				properties.receivebufferoffset <- (properties.receivebufferoffset + length + 4 + 3) land (lnot 3);
				(* try this... *)
				if properties.receivebufferoffset > (0x10000+16) then
					properties.receivebufferoffset <- properties.receivebufferoffset - 0x10000;
				if properties.receivebufferoffset < 16 then
					Vt100.printf "rtl: writing negative value to capr\n";
				out16 Registers.capr (properties.receivebufferoffset - 16); (* what happens if this is negative? *)
				(* send received packet to the rx_buffer *)
				Event.sync (Event.send rx_buffer (packet,0));
			with Break -> Vt100.printf "rtl.read error!\r\n" | Restart -> read properties rx_buffer
		
		let rec isr properties rx_buffer () =
			try
				let isr_contents = in16 Registers.isr in
				if isr_contents = 0 then raise Break;
				if isr_contents land InterruptStatusBits.receiveok <> 0 then begin
					read properties rx_buffer;
				end;
				if isr_contents land InterruptStatusBits.transmitok <> 0 then begin
					(* with cv never being signalled, if we had more than 4 calls to send above
					   simultaneously, the driver would block *)
					(*Mutex.lock m;*)
					let transmitid = properties.finished_packets mod 4 in
					properties.transmitbusy.(transmitid) <- false;
					properties.writes <- properties.writes - 1;
					properties.finished_packets <- properties.finished_packets + 1;
					(*Condition.signal cv;
					Mutex.unlock m;*)
				end;
				out16 Registers.isr isr_contents;
				isr properties rx_buffer (); (* maybe we should just let this return... *)
			with Break -> ()
		
		let address properties = match properties.address with
			| [a;b;c;d;e;f] -> NetworkProtocolStack.Ethernet.Addr (a,b,c,d,e,f)
			| _ -> failwith "Invalid MAC address"
	end in
	let module Driver = EthernetDriver(RTL8139) in
	let net_device = EthernetStack.create Driver.init Driver.read Driver.write pcii.request_line Driver.address in
	NetworkStack.register_device net_device

let init () =
	DeviceManager.add_driver "RealTek 8139" create 0x10EC 0x8139
