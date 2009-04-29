
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

(* assume regAddr is resources.(0) *)

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

(*raise_ee_clock
lower_ee_clock
shift_out_ee_bits
shift_in_ee_bits
acquire_eeprom
release_eeprom
standby_eeprom
read_eeprom*)

(*#define E1000_READ_OFFSET(hw, offset) \
        (*(volatile uint32 *)((char * )(((struct em_osdep * )(hw)->back)->dev->regAddr) + offset))
#define E1000_WRITE_OFFSET(hw, offset, value) \
        (*(volatile uint32 * )((char * )(((struct em_osdep * )(hw)->back)->dev->regAddr) + offset) = value)
#define E1000_REG_OFFSET(hw, reg) \
    ((hw)->mac_type >= em_82543 ? E1000_##reg : E1000_82542_##reg)
    
E1000_READ_REG
#define E1000_READ_REG(hw, reg) \
    E1000_READ_OFFSET(hw, E1000_REG_OFFSET(hw, reg))
E1000_WRITE_REG
#define E1000_WRITE_REG(hw, reg, value) \
    E1000_WRITE_OFFSET(hw, E1000_REG_OFFSET(hw, reg), value)
E1000_WRITE_FLUSH
#define E1000_WRITE_FLUSH(a) \
        E1000_READ_REG(a, STATUS) *)*)
        
let usec_delay () =
    for i = 1 to EEPROM.delay_usec do
        Asm.out8 0x80 0x80
    done

let read_reg = Asm.peek32_offset
let write_reg = Asm.poke32_offset
let write_flush eeprom = ignore (Asm.peek32_offset eeprom E1000.status)

let raise_ee_clk eeprom eecd =
    let eecd = eecd |! E1000.EECD.sk in
    write_reg eeprom E1000.eecd eecd;
    write_flush eeprom;
    usec_delay ();
    eecd

let lower_ee_clk eeprom eecd =
    let eecd = eecd &! ~!E1000.EECD.sk in
    write_reg eeprom E1000.eecd eecd;
    write_flush eeprom;
    usec_delay ();
    eecd

let rec accum n f r = if n = 0 then r else accum (n-1) f (f r)

let shift_out_ee_bits eeprom data count =
    let (eecd, _) = accum count begin fun (eecd,mask) ->
        let eecd = eecd &! ~!E1000.EECD.di in
        let eecd =
            if data land mask <> 0 then eecd |! E1000.EECD.di else eecd in
        write_reg eeprom E1000.eecd eecd;
        write_flush eeprom;
        usec_delay ();
        ((lower_ee_clk eeprom (raise_ee_clk eeprom eecd)), mask lsr 1)
    end (((read_reg eeprom E1000.eecd) &! ~!E1000.EECD._do), 1 lsl (count - 1)) in
    (* we leave the DI bit set to 0 when we leave this routine *)
    write_reg eeprom E1000.eecd (eecd &! ~!E1000.EECD.di)

let shift_in_ee_bits eeprom count =
    let _, data = accum count begin fun (eecd, data) ->
        let data = data lsl 1 in
        ignore (raise_ee_clk eeprom eecd);
        let eecd = read_reg eeprom E1000.eecd in
        let eecd = eecd &! ~!E1000.EECD.di in
        let data =
            if eecd &! E1000.EECD._do <> zero then data lor 1 else data in
        lower_ee_clk eeprom eecd, data
    end (((read_reg eeprom E1000.eecd) &! ~!(E1000.EECD._do |! E1000.EECD.di)), 0) in
    data

let standby_eeprom eeprom =
    let eecd = read_reg eeprom E1000.eecd in
    let x = [
        (fun eecd -> eecd &! ~!(E1000.EECD.cs |! E1000.EECD.sk));
        (fun eecd -> eecd |! E1000.EECD.sk);  (* clock high *)
        (fun eecd -> eecd |! E1000.EECD.cs);  (* select eeprom *)
        (fun eecd -> eecd &! ~!E1000.EECD.sk) (* clock low *)
    ] in
    ignore (List.fold_left (fun eecd f ->
        let eecd = f eecd in
        write_reg eeprom E1000.eecd eecd;
        write_flush eeprom;
        usec_delay ();
        eecd) eecd x)
        
let acquire_eeprom eeprom =
    let eecd = read_reg eeprom E1000.eecd in
    (* request eeprom access *)
    let eecd = eecd |! E1000.EECD.req in
    write_reg eeprom E1000.eecd eecd;
    let limit = ref 0 in
    while ((read_reg eeprom E1000.eecd) &! E1000.EECD.gnt = zero) && !limit < E1000.grant_attempts do
        incr limit;
        (* delay 5usec *)
        usec_delay ();
    done;
    if (read_reg eeprom E1000.eecd) &! E1000.EECD.gnt = zero then begin
        write_reg eeprom E1000.eecd (eecd &! ~!E1000.EECD.req);
        failwith "Could not acquire EEPROM grant";
    end;
    (* setup eeprom for read/write *)
    (* clear sk and di *)
    let eecd = eecd &! ~!(E1000.EECD.di |! E1000.EECD.sk) in
    write_reg eeprom E1000.eecd eecd;
    (* set cs *)
    let eecd = eecd |! E1000.EECD.cs in
    write_reg eeprom E1000.eecd eecd
    (* done! *)

(* read eeprom *)

let read_eeprom eeprom offset words =
    if offset > EEPROM.word_size || words > EEPROM.word_size - offset || words = 0 then
        failwith "Invalid parameters supplied";
    acquire_eeprom eeprom;
    let data = Array.make words 0 in
    (* assuming microwire eeprom *)
    for i = 0 to words - 1 do
        shift_out_ee_bits eeprom E1000.read_opcode EEPROM.opcode_bits;
        shift_out_ee_bits eeprom (offset + i) EEPROM.address_bits;
        data.(i) <- shift_in_ee_bits eeprom 16;
        standby_eeprom eeprom;
    done;
    (*release_eeprom eeprom;*)
    data

(* read mac address *)

let read_mac_addr eeprom =
    let mac_addr = [| 0; 0; 0; 0; 0; 0 |] in
    for i = 0 to 2 do (* 0 to node_address_size / 2 - 1 *)
        let data = read_eeprom eeprom ((i * 2) lsr 1) 1 in
        mac_addr.(i * 2) <- data.(0) land 0x00FF;
        mac_addr.(i * 2 + 1) <- data.(0) lsr 8;
    done;
    mac_addr

(* create the driver instance *)

let get_resource device = match device.resources.(0) with
    | Memory mem -> mem
    | _ -> failwith "Invalid resource type"

let create device =
    let resource = get_resource device in
    Asm.poke32 resource ((Asm.peek32 resource) |! 0x40l);
    let mac_addr = read_mac_addr resource in
    Vt100.printf "mac_addr: %02x:%02x:%02x:%02x:%02x:%02x\n"
        mac_addr.(0) mac_addr.(1) mac_addr.(2)
        mac_addr.(3) mac_addr.(4) mac_addr.(5);
    ()

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
    