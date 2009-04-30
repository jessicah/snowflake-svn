
open PCI

module Int32Ops = struct
	let ( +! ) = Int32.add
	let ( -! ) = Int32.sub
	let ( *! ) = Int32.mul
	let ( /! ) = Int32.div
	let ( &! ) = Int32.logand
	let ( |! ) = Int32.logor
	let ( >>! ) = Int32.shift_right_logical
	let ( <<! ) = Int32.shift_left
	let ( ~! ) = Int32.lognot
	let ( ^! ) = Int32.logxor
	let zero = Int32.zero
	let one = Int32.one
	let minus_one = Int32.minus_one
	let neg = Int32.neg
	let rem = Int32.rem
	let of_int = Int32.of_int
end

open Int32Ops

module EEPROM = struct
    (* microwire *)
    let opcode_bits = 3
    let delay_usec = 50
    let word_size = 64
    let address_bits = 6
end

let node_address_size = 6

module E1000 = struct
    let status = 0x08
    module EECD = struct
        let sk = 0x01l
        let cs = 0x02l
        let di = 0x04l
        let _do = 0x08l
        let req = 0x40l
        let gnt = 0x80l
    end
    let eecd = 0x10
    let grant_attempts = 1000
    let read_opcode = 0x6
end

let usec_delay () =
    for i = 1 to EEPROM.delay_usec do
        Asm.out8 0x80 0x80
    done

let get_resource device = match device.resources.(0) with
    | Memory mem -> mem
    | _ -> failwith "Invalid resource type"

let (>>=) x f = f x
let wrap f x = f x; x
let lift f x = f (); x

let rec accum n f r = if n = 0 then r else accum (n-1) f (f r)
let rec accum_n n f r = if n = 0 then r else accum_n (n-1) f (f n r)

(* create the driver instance *)

let create device =
    (* get the location of the EEPROM *)
    let eeprom = get_resource device in
    (* and some helper functions to read/write to the EEPROM *)
    let read () = Asm.peek32_offset eeprom E1000.eecd in
    let write value = Asm.poke32_offset eeprom E1000.eecd value in
    let flush () = ignore (Asm.peek32_offset eeprom E1000.status) in
    let write_delay value = value >>= wrap write >>= lift flush >>= lift usec_delay in
    (* nic stuff, done here to use the helper methods *)
    let module E = struct
        let raise_clock eecd =
            eecd |! E1000.EECD.sk >>= write_delay
        
        let lower_clock eecd =
            eecd &! ~!E1000.EECD.sk >>= write_delay
        
        let standby () =
            let k1 = fun x -> x &! ~!(E1000.EECD.cs |! E1000.EECD.sk)
            and k2 = fun x -> x |! E1000.EECD.sk
            and k3 = fun x -> x |! E1000.EECD.cs
            and k4 = fun x -> x &! ~!E1000.EECD.sk in
            read () >>= k1 >>= write_delay >>= k2 >>= write_delay >>=
            k3 >>= write_delay >>= k4 >>= write_delay >>= ignore
        
        let output data count =
            let bits = Array.init count begin fun n -> data land (1 lsl n) <> 0 end in
            (read ()) &! ~!E1000.EECD._do >>=
            accum_n count begin fun n x ->
                (* need to start at right-most array entry, so bits.(n-1) will work *)
                (if bits.(n-1) then x |! E1000.EECD.di else x &! ~!E1000.EECD.di) >>=
                write_delay >>= raise_clock >>= lower_clock
            end >>=
            (fun x -> x &! ~!E1000.EECD.di) >>=
            write
        
        let input count =
            let data = ref 0 in
            (read ()) &! ~!(E1000.EECD._do |! E1000.EECD.di) >>=
            accum count begin fun x ->
                data := !data lsl 1;
                ignore (raise_clock x);
                let x = (read ()) &! ~!E1000.EECD.di in
                if x &! E1000.EECD._do <> zero then data := !data lor 1;
                lower_clock x
            end >>= ignore;
            !data
        
        let acquire () =
            read () |! E1000.EECD.req >>= wrap write >>= begin fun x ->
                let limit = ref 0 in
                while read () &! E1000.EECD.gnt = zero && !limit < E1000.grant_attempts do
                    incr limit; usec_delay ();
                done;
                if !limit = E1000.grant_attempts then begin
                    write (x &! ~!E1000.EECD.req);
                    failwith "e1000: unable to acquire EEPROM";
                end;
                x &! ~!(E1000.EECD.di |! E1000.EECD.sk)
            end >>=
            wrap write >>=
            begin fun x -> x |! E1000.EECD.cs end >>=
            write
        
        let read_word offset = (* special case when reading single word *)
            (* sanity checking omitted *)
            acquire ();
            output E1000.read_opcode EEPROM.opcode_bits;
            output offset EEPROM.address_bits;
            input 16 >>= lift standby
            (* we still need to release the EEPROM :P *)
        end
    in
    (* get the mac address *)
    let mac_addr = [| 0; 0; 0; 0; 0; 0 |] in
    for i = 0 to 2 do
        let data = E.read_word ((i * 2) lsr 1) in
        mac_addr.(i*2)   <- data land 0xFF;
        mac_addr.(i*2+1) <- data lsr 8;
    done;
    (* print the mac address *)
    Vt100.printf "e1000: mac addr: %02x:%02x:%02x:%02x:%02x:%02x\n"
        mac_addr.(0) mac_addr.(1) mac_addr.(2)
        mac_addr.(3) mac_addr.(4) mac_addr.(5)

(* init stuff *)

let init () =
    List.iter begin fun dev_id ->
        DeviceManager.add_driver "Intel E1000" create 0x8086 dev_id
    end [
        0x100E; 0x100F; 0x1010; 0x1010; 0x1011; 0x1012; 0x1013;
        0x1015; 0x1016; 0x1017; 0x1018; 0x1019; 0x101A; 0x101D;
        0x1026; 0x1027; 0x1028; 0x1076; 0x1077; 0x1078; 0x1079;
        0x107A; 0x107B; 0x1107; 0x1112;
    ]
    