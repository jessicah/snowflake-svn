(* ALAC : Decoder *)
let failf args = Printf.kprintf failwith args
  
module Printf = struct let printf args = Printf.kprintf ignore args
                          end
  
type element = | SCE | CPE | CCE | LFE | DSE | PCE | FIL | END

(* single channel element *)
(* channel pair element *)
(* coupling channel element *)
(* LFE channel element *)
let of_element =
  function
  | SCE -> 0
  | CPE -> 1
  | CCE -> 2
  | LFE -> 3
  | DSE -> 4
  | PCE -> 5
  | FIL -> 6
  | END -> 7
  
let to_element x =
  match x with
  | 0 -> SCE
  | 1 -> CPE
  | 2 -> CCE
  | 3 -> LFE
  | 4 -> DSE
  | 5 -> PCE
  | 6 -> FIL
  | 7 -> END
  | n -> failf "invalid element type: %d" n
  
(* link in the other modules until have a full decoder and don't need this hack :p *)
(*open BitBuffer
open ArrayTypes
open DynamicPredictor
open Matrix
open AdaptiveGolomb*)
(* ALACDecoder.h
	int32 Init (void* inMagicCookier, uint32_t inMagicCookieSize)
	int32 Decode (struct BitBuffer *bits, uint8_t *sampleBuffer, uint32_t numSamples, uint32_t numChannels, uint32_t *outNumSamples)
	
	ALACSpecificConfig mConfig
	
	int32_t FillElement (struct BitBuffer *bits)
	int32_t DataStreamElement (struct BitBuffer *bits)
	
	uint16_t mActiveElements
	
	int32_t *mMixBufferU
	int32_t *mMixBufferV
	int32_t *mPredictor
	uint16_t *mShiftBuffer /* this is a sub-array of mPredictor */
*)
type specific_config =
  { frame_length : int32; (* uint32 *)
    (*compatible_version : int; (* uint8 *) -- has no real use for us *)
    bit_depth : int; (* uint8 *) pb : int; (* uint8 *) mb : int; (* uint8 *)
    kb : int; (* uint8 *) num_channels : int; (* uint8 *) max_run : int;
    (* uint16 *) max_frame_bytes : int32; (* uint32 *) avg_bit_rate : int32;
    (* uint32 *) sample_rate : int32
  }

(* uint32 *)
let print_specific_config cfg =
  Printf.printf
    ("frame length:    %ld\n" ^^
       ("bit depth:       %d\n" ^^
          ("pb:              %d\n" ^^
             ("mb:              %d\n" ^^
                ("kb:              %d\n" ^^
                   ("channels:        %d\n" ^^
                      ("max run:         %d\n" ^^
                         ("max frame bytes: %ld\n" ^^
                            ("avg bitrate:     %ld\n" ^^
                               "sample rate:     %ld\n")))))))))
    cfg.frame_length cfg.bit_depth cfg.pb cfg.mb cfg.kb cfg.num_channels
    cfg.max_run cfg.max_frame_bytes cfg.avg_bit_rate cfg.sample_rate
  
let rec init bits =
  let (__pabitstring_data_1001, __pabitstring_original_off_1004,
       __pabitstring_original_len_1005) =
    bits in
  let __pabitstring_off_1002 = __pabitstring_original_off_1004
  and __pabitstring_len_1003 = __pabitstring_original_len_1005 in
  let __pabitstring_off_aligned_1006 = (__pabitstring_off_1002 land 7) = 0
  in
    (ignore __pabitstring_off_aligned_1006;
     let __pabitstring_result_1007 = ref None
     in
       ((try
           (if __pabitstring_len_1003 >= 32
            then
              (let _ =
                 (__pabitstring_data_1001, __pabitstring_off_1002, 32) in
               let __pabitstring_off_1002 = __pabitstring_off_1002 + 32
               and __pabitstring_len_1003 = __pabitstring_len_1003 - 32
               in
                 if __pabitstring_len_1003 >= 32
                 then
                   (let str =
                      if __pabitstring_off_aligned_1006
                      then
                        (let o = (__pabitstring_original_off_1004 lsr 3) + 4
                         in String.sub __pabitstring_data_1001 o 4)
                      else
                        Bitstring.string_of_bitstring
                          (__pabitstring_data_1001, __pabitstring_off_1002,
                           32) in
                    let __pabitstring_off_1002 = __pabitstring_off_1002 + 32
                    and __pabitstring_len_1003 = __pabitstring_len_1003 - 32
                    in
                      match str with
                      | "frma" when true ->
                          if __pabitstring_len_1003 >= 32
                          then
                            (let _ =
                               (__pabitstring_data_1001,
                                __pabitstring_off_1002, 32) in
                             let __pabitstring_off_1002 =
                               __pabitstring_off_1002 + 32
                             and __pabitstring_len_1003 =
                               __pabitstring_len_1003 - 32 in
                             let rest =
                               (__pabitstring_data_1001,
                                __pabitstring_off_1002,
                                __pabitstring_len_1003) in
                             let __pabitstring_off_1002 =
                               __pabitstring_off_1002 +
                                 __pabitstring_len_1003 in
                             let __pabitstring_len_1003 = 0
                             in
                               (__pabitstring_result_1007 :=
                                  Some
                                    (* skip format ('frma') atom if present *)
                                    (init rest);
                                raise Exit))
                          else ()
                      | _ -> ())
                 else ())
            else ();
            if __pabitstring_len_1003 >= 32
            then
              (let _ =
                 (__pabitstring_data_1001, __pabitstring_off_1002, 32) in
               let __pabitstring_off_1002 = __pabitstring_off_1002 + 32
               and __pabitstring_len_1003 = __pabitstring_len_1003 - 32
               in
                 if __pabitstring_len_1003 >= 32
                 then
                   (let str =
                      if __pabitstring_off_aligned_1006
                      then
                        (let o = (__pabitstring_original_off_1004 lsr 3) + 4
                         in String.sub __pabitstring_data_1001 o 4)
                      else
                        Bitstring.string_of_bitstring
                          (__pabitstring_data_1001, __pabitstring_off_1002,
                           32) in
                    let __pabitstring_off_1002 = __pabitstring_off_1002 + 32
                    and __pabitstring_len_1003 = __pabitstring_len_1003 - 32
                    in
                      match str with
                      | "alac" when true ->
                          if __pabitstring_len_1003 >= 32
                          then
                            (let _ =
                               (__pabitstring_data_1001,
                                __pabitstring_off_1002, 32) in
                             let __pabitstring_off_1002 =
                               __pabitstring_off_1002 + 32
                             and __pabitstring_len_1003 =
                               __pabitstring_len_1003 - 32 in
                             let rest =
                               (__pabitstring_data_1001,
                                __pabitstring_off_1002,
                                __pabitstring_len_1003) in
                             let __pabitstring_off_1002 =
                               __pabitstring_off_1002 +
                                 __pabitstring_len_1003 in
                             let __pabitstring_len_1003 = 0
                             in
                               (__pabitstring_result_1007 :=
                                  Some
                                    (* skip 'alac' atom header if present *)
                                    (init rest);
                                raise Exit))
                          else ()
                      | _ -> ())
                 else ())
            else ();
            if __pabitstring_len_1003 >= 32
            then
              (let v =
                 if __pabitstring_off_aligned_1006
                 then
                   (let o = (__pabitstring_original_off_1004 lsr 3) + 0 in
                    let zero = Int32.of_int 0
                    in
                      Bitstring.extract_fastpath_int32_be_unsigned
                        __pabitstring_data_1001 o zero)
                 else
                   Bitstring.extract_int32_be_unsigned
                     __pabitstring_data_1001 __pabitstring_off_1002
                     __pabitstring_len_1003 32 in
               let __pabitstring_off_1002 = __pabitstring_off_1002 + 32
               and __pabitstring_len_1003 = __pabitstring_len_1003 - 32
               in
                 match v with
                 | frame_length when true ->
                     if __pabitstring_len_1003 >= 8
                     then
                       (let v =
                          if __pabitstring_off_aligned_1006
                          then
                            (let o =
                               (__pabitstring_original_off_1004 lsr 3) + 4
                             in
                               Char.code
                                 (String.unsafe_get __pabitstring_data_1001 o))
                          else
                            Bitstring.extract_char_unsigned
                              __pabitstring_data_1001 __pabitstring_off_1002
                              __pabitstring_len_1003 8 in
                        let __pabitstring_off_1002 =
                          __pabitstring_off_1002 + 8
                        and __pabitstring_len_1003 =
                          __pabitstring_len_1003 - 8
                        in
                          match v with
                          | compatible_version when true ->
                              if __pabitstring_len_1003 >= 8
                              then
                                (let v =
                                   if __pabitstring_off_aligned_1006
                                   then
                                     (let o =
                                        (__pabitstring_original_off_1004 lsr
                                           3)
                                          + 5
                                      in
                                        Char.code
                                          (String.unsafe_get
                                             __pabitstring_data_1001 o))
                                   else
                                     Bitstring.extract_char_unsigned
                                       __pabitstring_data_1001
                                       __pabitstring_off_1002
                                       __pabitstring_len_1003 8 in
                                 let __pabitstring_off_1002 =
                                   __pabitstring_off_1002 + 8
                                 and __pabitstring_len_1003 =
                                   __pabitstring_len_1003 - 8
                                 in
                                   match v with
                                   | bit_depth when true ->
                                       if __pabitstring_len_1003 >= 8
                                       then
                                         (let v =
                                            if __pabitstring_off_aligned_1006
                                            then
                                              (let o =
                                                 (__pabitstring_original_off_1004
                                                    lsr 3)
                                                   + 6
                                               in
                                                 Char.code
                                                   (String.unsafe_get
                                                      __pabitstring_data_1001
                                                      o))
                                            else
                                              Bitstring.extract_char_unsigned
                                                __pabitstring_data_1001
                                                __pabitstring_off_1002
                                                __pabitstring_len_1003 8 in
                                          let __pabitstring_off_1002 
                                            = __pabitstring_off_1002 + 8
                                          and __pabitstring_len_1003 
                                            = __pabitstring_len_1003 - 8
                                          in
                                            match v with
                                            | pb when true ->
                                                if
                                                  __pabitstring_len_1003 >= 8
                                                then
                                                  (let v =
                                                     if
                                                       __pabitstring_off_aligned_1006
                                                     then
                                                       (let o =
                                                          (__pabitstring_original_off_1004
                                                             lsr 3)
                                                            + 7
                                                        in
                                                          Char.code
                                                            (String.
                                                               unsafe_get
                                                               __pabitstring_data_1001
                                                               o))
                                                     else
                                                       Bitstring.
                                                         extract_char_unsigned
                                                         __pabitstring_data_1001
                                                         __pabitstring_off_1002
                                                         __pabitstring_len_1003
                                                         8 in
                                                   let __pabitstring_off_1002 
                                                     =
                                                     __pabitstring_off_1002 +
                                                       8
                                                   and
                                                     __pabitstring_len_1003 
                                                     =
                                                     __pabitstring_len_1003 -
                                                       8
                                                   in
                                                     match v with
                                                     | mb when true ->
                                                         if
                                                           __pabitstring_len_1003
                                                             >= 8
                                                         then
                                                           (let v =
                                                              if
                                                                __pabitstring_off_aligned_1006
                                                              then
                                                                (let o 
                                                                   =
                                                                   (__pabitstring_original_off_1004
                                                                    lsr 3) +
                                                                    8
                                                                 in
                                                                   Char.code
                                                                    (String.
                                                                    unsafe_get
                                                                    __pabitstring_data_1001
                                                                    o))
                                                              else
                                                                Bitstring.
                                                                  extract_char_unsigned
                                                                  __pabitstring_data_1001
                                                                  __pabitstring_off_1002
                                                                  __pabitstring_len_1003
                                                                  8 in
                                                            let __pabitstring_off_1002 
                                                              =
                                                              __pabitstring_off_1002
                                                                + 8
                                                            and
                                                              __pabitstring_len_1003 
                                                              =
                                                              __pabitstring_len_1003
                                                                - 8
                                                            in
                                                              match v with
                                                              | kb when true
                                                                  ->
                                                                  if
                                                                    __pabitstring_len_1003
                                                                    >= 8
                                                                  then
                                                                    (
                                                                    let v 
                                                                    =
                                                                    if
                                                                    __pabitstring_off_aligned_1006
                                                                    then
                                                                    (let o 
                                                                    =
                                                                    (__pabitstring_original_off_1004
                                                                    lsr 3) +
                                                                    9
                                                                    in
                                                                    Char.code
                                                                    (String.
                                                                    unsafe_get
                                                                    __pabitstring_data_1001
                                                                    o))
                                                                    else
                                                                    Bitstring.
                                                                    extract_char_unsigned
                                                                    __pabitstring_data_1001
                                                                    __pabitstring_off_1002
                                                                    __pabitstring_len_1003
                                                                    8 in
                                                                    let __pabitstring_off_1002 
                                                                    =
                                                                    __pabitstring_off_1002
                                                                    + 8
                                                                    and
                                                                    __pabitstring_len_1003 
                                                                    =
                                                                    __pabitstring_len_1003
                                                                    - 8
                                                                    in
                                                                    match v
                                                                    with
                                                                    | 
                                                                    num_channels
                                                                    when true
                                                                    ->
                                                                    if
                                                                    __pabitstring_len_1003
                                                                    >= 16
                                                                    then
                                                                    (let v 
                                                                    =
                                                                    if
                                                                    __pabitstring_off_aligned_1006
                                                                    then
                                                                    (let o 
                                                                    =
                                                                    (__pabitstring_original_off_1004
                                                                    lsr 3) +
                                                                    10
                                                                    in
                                                                    Bitstring.
                                                                    extract_fastpath_int16_be_unsigned
                                                                    __pabitstring_data_1001
                                                                    o)
                                                                    else
                                                                    Bitstring.
                                                                    extract_int_be_unsigned
                                                                    __pabitstring_data_1001
                                                                    __pabitstring_off_1002
                                                                    __pabitstring_len_1003
                                                                    16 in
                                                                    let __pabitstring_off_1002 
                                                                    =
                                                                    __pabitstring_off_1002
                                                                    + 16
                                                                    and
                                                                    __pabitstring_len_1003 
                                                                    =
                                                                    __pabitstring_len_1003
                                                                    - 16
                                                                    in
                                                                    match v
                                                                    with
                                                                    | 
                                                                    max_run
                                                                    when true
                                                                    ->
                                                                    if
                                                                    __pabitstring_len_1003
                                                                    >= 32
                                                                    then
                                                                    (let v 
                                                                    =
                                                                    if
                                                                    __pabitstring_off_aligned_1006
                                                                    then
                                                                    (let o 
                                                                    =
                                                                    (__pabitstring_original_off_1004
                                                                    lsr 3) +
                                                                    12 in
                                                                    let zero 
                                                                    =
                                                                    Int32.
                                                                    of_int 0
                                                                    in
                                                                    Bitstring.
                                                                    extract_fastpath_int32_be_unsigned
                                                                    __pabitstring_data_1001
                                                                    o zero)
                                                                    else
                                                                    Bitstring.
                                                                    extract_int32_be_unsigned
                                                                    __pabitstring_data_1001
                                                                    __pabitstring_off_1002
                                                                    __pabitstring_len_1003
                                                                    32 in
                                                                    let __pabitstring_off_1002 
                                                                    =
                                                                    __pabitstring_off_1002
                                                                    + 32
                                                                    and
                                                                    __pabitstring_len_1003 
                                                                    =
                                                                    __pabitstring_len_1003
                                                                    - 32
                                                                    in
                                                                    match v
                                                                    with
                                                                    | 
                                                                    max_frame_bytes
                                                                    when true
                                                                    ->
                                                                    if
                                                                    __pabitstring_len_1003
                                                                    >= 32
                                                                    then
                                                                    (let v 
                                                                    =
                                                                    if
                                                                    __pabitstring_off_aligned_1006
                                                                    then
                                                                    (let o 
                                                                    =
                                                                    (__pabitstring_original_off_1004
                                                                    lsr 3) +
                                                                    16 in
                                                                    let zero 
                                                                    =
                                                                    Int32.
                                                                    of_int 0
                                                                    in
                                                                    Bitstring.
                                                                    extract_fastpath_int32_be_unsigned
                                                                    __pabitstring_data_1001
                                                                    o zero)
                                                                    else
                                                                    Bitstring.
                                                                    extract_int32_be_unsigned
                                                                    __pabitstring_data_1001
                                                                    __pabitstring_off_1002
                                                                    __pabitstring_len_1003
                                                                    32 in
                                                                    let __pabitstring_off_1002 
                                                                    =
                                                                    __pabitstring_off_1002
                                                                    + 32
                                                                    and
                                                                    __pabitstring_len_1003 
                                                                    =
                                                                    __pabitstring_len_1003
                                                                    - 32
                                                                    in
                                                                    match v
                                                                    with
                                                                    | 
                                                                    avg_bit_rate
                                                                    when true
                                                                    ->
                                                                    if
                                                                    __pabitstring_len_1003
                                                                    >= 32
                                                                    then
                                                                    (let v 
                                                                    =
                                                                    if
                                                                    __pabitstring_off_aligned_1006
                                                                    then
                                                                    (let o 
                                                                    =
                                                                    (__pabitstring_original_off_1004
                                                                    lsr 3) +
                                                                    20 in
                                                                    let zero 
                                                                    =
                                                                    Int32.
                                                                    of_int 0
                                                                    in
                                                                    Bitstring.
                                                                    extract_fastpath_int32_be_unsigned
                                                                    __pabitstring_data_1001
                                                                    o zero)
                                                                    else
                                                                    Bitstring.
                                                                    extract_int32_be_unsigned
                                                                    __pabitstring_data_1001
                                                                    __pabitstring_off_1002
                                                                    __pabitstring_len_1003
                                                                    32 in
                                                                    let __pabitstring_off_1002 
                                                                    =
                                                                    __pabitstring_off_1002
                                                                    + 32
                                                                    and
                                                                    __pabitstring_len_1003 
                                                                    =
                                                                    __pabitstring_len_1003
                                                                    - 32
                                                                    in
                                                                    match v
                                                                    with
                                                                    | 
                                                                    sample_rate
                                                                    when true
                                                                    ->
                                                                    if
                                                                    (* ensure version matches *)
                                                                    compatible_version
                                                                    = 0
                                                                    then
                                                                    (__pabitstring_result_1007 :=
                                                                    Some
                                                                    (* return the specific_config... the buffers don't matter too much right now *)
                                                                    {
                                                                    frame_length =
                                                                    frame_length;
                                                                    bit_depth =
                                                                    bit_depth;
                                                                    pb = pb;
                                                                    mb = mb;
                                                                    kb = kb;
                                                                    num_channels =
                                                                    num_channels;
                                                                    max_run =
                                                                    max_run;
                                                                    max_frame_bytes =
                                                                    max_frame_bytes;
                                                                    avg_bit_rate =
                                                                    avg_bit_rate;
                                                                    sample_rate =
                                                                    sample_rate;
                                                                    };
                                                                    raise
                                                                    Exit)
                                                                    else ()
                                                                    | 
                                                                    _ -> ())
                                                                    else ()
                                                                    | 
                                                                    _ -> ())
                                                                    else ()
                                                                    | 
                                                                    _ -> ())
                                                                    else ()
                                                                    | 
                                                                    _ -> ())
                                                                    else ()
                                                                    | 
                                                                    _ -> ())
                                                                  else ()
                                                              | _ -> ())
                                                         else ()
                                                     | _ -> ())
                                                else ()
                                            | _ -> ())
                                       else ()
                                   | _ -> ())
                              else ()
                          | _ -> ())
                     else ()
                 | _ -> ())
            else ();
            __pabitstring_result_1007 :=
              Some (failwith "alac: missing/invalid cookie");
            raise Exit)
         with | Exit -> ());
        match !__pabitstring_result_1007 with
        | Some x -> x
        | None -> raise (Match_failure ("alac.mlp", 95, 1))))
  
let fill_element bits =
  let count =
    match BitBuffer.read_small bits 4 with
    | 15 -> (15 + (BitBuffer.read_small bits 8)) - 1
    | n -> n
  in BitBuffer.advance bits (count * 8)
  
let data_stream_element bits =
  let _ = (* element_instance_tag *) BitBuffer.read_small bits 4 in
  let data_byte_align_flag = BitBuffer.read_one bits in
  let count =
    match BitBuffer.read_small bits 8 with
    | 255 -> 255 + (BitBuffer.read_small bits 8)
    | n -> n
  in
    (if data_byte_align_flag <> 0
     then BitBuffer.byte_align bits false
     else ();
     BitBuffer.advance bits (count * 8))
  
let zero16 (buffer : ArrayTypes.int16a) num_items stride =
  failwith "alac: shouldn't need; only dealing with stereo files"
  
(* globals *)
open Bigarray
  
let to_hex2 arr =
  (for i = 0 to min ((Array1.dim arr) - 1) ((16 * 8) - 1) do
     if (i mod 16) = 0 then Printf.printf "\n" else ();
     Printf.printf "%02x " (Bigarray.Array1.get arr i)
   done;
   Printf.printf "\n\n")
  
let to_hex arr =
  (for i = 0 to (Array1.dim arr) - 1 do
     if (i mod 16) = 0 then Printf.printf "\n" else ();
     Printf.printf "%02x " (Bigarray.Array1.get arr i)
   done;
   Printf.printf "\n")
  
let config =
  ref
    {
      frame_length = 0l;
      bit_depth = 0;
      pb = 0;
      mb = 0;
      kb = 0;
      num_channels = 0;
      max_run = 0;
      max_frame_bytes = 0l;
      avg_bit_rate = 0l;
      sample_rate = 0l;
    }
  
let mix_buffer_U = ref (Array1.create int32 c_layout 0)
  
let mix_buffer_V = ref (Array1.create int32 c_layout 0)
  
let predictor = ref (Array1.create int32 c_layout 0)
  
let shift_buffer = ref (Array1.create int16_unsigned c_layout 0)
  
exception Done
  
(* returns out_num_samples *)
(* since we're only supporting 16-bit audio, we
	can let sample_buffer be a 16-bit array type *)
(* unmix16 is only function that uses sample_buffer;
	takes in mix_buffers U & V *)
(* mix buffers are declared as int32 arrays, as is predictor *)
(* shift buffer is the predictor as an unsigned 16-bit array type *)
let decode bits (sample_buffer : ArrayTypes.uint8a) num_samples num_channels
           =
  let shift_bits = ref (BitBuffer.create "" 0) in
  let out_num_samples = ref 0 in
  let coefs_U = Array1.create int16_signed c_layout 32 in
  let coefs_V = Array1.create int16_signed c_layout 32 in
  let mix_bits = ref 0 in
  let mix_res = ref 0
  in
    try
      (*while true do*)
      let pb = !config.pb
      in
        (*Printf.printf "first few bytes:\n";
for i = 0 to 15 do
	Printf.printf "%02x " (Char.code bits.BitBuffer.buffer.[bits.BitBuffer.current+i]);
done;
Printf.printf "\n";*)
        ((match to_element (BitBuffer.read_small bits 3) with
          | CPE ->
              (Printf.printf "stereo channel pair...\n%!";
               (* stereo channel pair *)
               let _ = (* element_instance_tag *)
                 BitBuffer.read_small bits 4 in
               (* don't care about active elements *)
               (* read the 12 unused header bits *)
               let unused_header = BitBuffer.read bits 12
               in
                 (* assert = 0 *)
                 (assert (unused_header = 0);
                  (* read the 1-bit "partial frame" flag, 2-bit "shift-off" flag & 1-bit "escape" flag *)
                  let header_byte = BitBuffer.read bits 4 in
                  let partial_frame = header_byte lsr 3 in
                  let bytes_shifted = (header_byte lsr 1) land 0x3
                  in
                    (* assert != 3 *)
                    (assert (bytes_shifted <> 3);
                     (*let shift = bytes_shifted * 8 in (* unused *)*)
                     let escape_flag = header_byte land 0x1 in
                     let chan_bits = (16 - (bytes_shifted * 8)) + 1 in
                     (* check for partial frame length to override requested num_samples *)
                     let num_samples =
                       if partial_frame <> 0
                       then
                         (let override = (BitBuffer.read bits 16) lsl 16
                          in override lor (BitBuffer.read bits 16))
                       else num_samples
                     in
                       (* compressed frame, read rest of parameters *)
                       (* if shift active, skip the interleaved shifted values, but remember where they start *)
                       (* decompress and run predictor for "left" channel *)
                       (* set_ag_params( &agParams, mConfig.mb, (pb * pbFactorU) / 4, mConfig.kb, numSamples, numSamples, mConfig.maxRun ) *)
                       (* dyn_decomp( &agParams, bits, mPredictor, numSamples, chanBits, &bits1 ) *)
                       (* the special "num_active = 31" mode can be done in-place *)
                       (* decompress and run predictor for "right" channel -- U => V *)
                       (* uncompressed frame, copy data into the mix buffers to use common output code *)
                       (* !config.bit_depth *)
                       (* if chan_bits <= 16 *)
                       (* bits1 & bits2 serve no useful purpose *)
                       (* bytes_shifted <- 0, if escape_flag = 0 && bytes_shifted <> 0 *)
                       (* now read the shifted values into the shift buffer *)
                       (* if escape_flag <> 0, then don't need to shift *)
                       (if escape_flag = 0
                        then
                          (Printf.printf "compressed frame...\n%!";
                           mix_bits := BitBuffer.read bits 8;
                           mix_res := BitBuffer.read bits 8;
                           (let header_byte = BitBuffer.read bits 8 in
                            let mode_U = header_byte lsr 4 in
                            let den_shift_U = header_byte land 0xf in
                            let header_byte = BitBuffer.read bits 8 in
                            let pb_factor_U = header_byte lsr 5 in
                            let num_U = header_byte land 0x1f
                            in
                              (Printf.printf
                                 "reading %d coefficients for left channel\n%!"
                                 num_U;
                               for i = 0 to num_U - 1 do
                                 Bigarray.Array1.set coefs_U i
                                   (BitBuffer.read bits 16);
                                 Printf.printf "%04x "
                                   (Bigarray.Array1.get coefs_U i)
                               done;
                               Printf.printf "\n";
                               let header_byte = BitBuffer.read bits 8 in
                               let mode_V = header_byte lsr 4 in
                               let den_shift_V = header_byte land 0xf in
                               let header_byte = BitBuffer.read bits 8 in
                               let pb_factor_V = header_byte lsr 5 in
                               let num_V = header_byte land 0x1f
                               in
                                 (Printf.printf
                                    "reading %d coefficients for right channel\n%!"
                                    num_V;
                                  for i = 0 to num_V - 1 do
                                    Bigarray.Array1.set coefs_V i
                                      (BitBuffer.read bits 16);
                                    Printf.printf "%04x "
                                      (Bigarray.Array1.get coefs_V i)
                                  done;
                                  Printf.printf "\n";
                                  if bytes_shifted <> 0
                                  then
                                    (shift_bits := BitBuffer.copy bits;
                                     BitBuffer.advance bits
                                       (((bytes_shifted * 8) * 2) *
                                          num_samples))
                                  else ();
                                  let ag_params =
                                    AdaptiveGolomb.make_params !config.mb
                                      ((pb * pb_factor_U) / 4) !config.kb
                                      num_samples num_samples !config.max_run
                                  in
                                    (Printf.printf
                                       "decompress and run predictor for left channel...\n%!";
                                     AdaptiveGolomb.dyn_decomp ag_params bits
                                       !predictor num_samples chan_bits;
                                     to_hex2
                                       (BigarrayUtils.int32_to_uint8
                                          !predictor);
                                     Printf.printf
                                       "unblocking... modeU = %d\n%!" mode_U;
                                     if mode_U = 0
                                     then
                                       (DynamicPredictor.unpc_block
                                          !predictor !mix_buffer_U
                                          num_samples coefs_U num_U chan_bits
                                          den_shift_U;
                                        to_hex2
                                          (BigarrayUtils.int32_to_uint8
                                             !mix_buffer_U))
                                     else
                                       (DynamicPredictor.unpc_block
                                          !predictor !predictor num_samples
                                          coefs_U 31 chan_bits 0;
                                        DynamicPredictor.unpc_block
                                          !predictor !mix_buffer_U
                                          num_samples coefs_U num_U chan_bits
                                          den_shift_U);
                                     let ag_params =
                                       AdaptiveGolomb.make_params !config.mb
                                         ((pb * pb_factor_V) / 4) !config.kb
                                         num_samples num_samples
                                         !config.max_run
                                     in
                                       (Printf.printf
                                          "decompress and run predictor for right channel...\n%!";
                                        AdaptiveGolomb.dyn_decomp ag_params
                                          bits !predictor num_samples
                                          chan_bits;
                                        to_hex2
                                          (BigarrayUtils.int32_to_uint8
                                             !predictor);
                                        Printf.printf
                                          "unblocking... modeV = %d\n%!"
                                          mode_U;
                                        if mode_V = 0
                                        then
                                          (DynamicPredictor.unpc_block
                                             !predictor !mix_buffer_V
                                             num_samples coefs_V num_V
                                             chan_bits den_shift_V;
                                           to_hex2
                                             (BigarrayUtils.int32_to_uint8
                                                !mix_buffer_V))
                                        else
                                          (DynamicPredictor.unpc_block
                                             !predictor !predictor
                                             num_samples coefs_V 31 chan_bits
                                             0;
                                           DynamicPredictor.unpc_block
                                             !predictor !mix_buffer_V
                                             num_samples coefs_V num_V
                                             chan_bits den_shift_V)))))))
                        else
                          (Printf.printf
                             "uncompressed frame... with %d samples\n%!"
                             num_samples;
                           (let chan_bits = 16 in
                            let shift = 32 - chan_bits
                            in
                              (for i = 0 to num_samples - 1 do
                                 (let value =
                                    Int32.of_int
                                      (BitBuffer.read bits chan_bits) in
                                  let value =
                                    Int32.shift_right
                                      (Int32.shift_left value shift) shift
                                  in
                                    (Bigarray.Array1.set !mix_buffer_U i
                                       value;
                                     let value =
                                       Int32.of_int
                                         (BitBuffer.read bits chan_bits) in
                                     let value =
                                       Int32.shift_right
                                         (Int32.shift_left value shift) shift
                                     in
                                       Bigarray.Array1.set !mix_buffer_V i
                                         value))
                               done;
                               mix_bits := 0;
                               mix_res := 0)));
                        if (escape_flag = 0) && (bytes_shifted <> 0)
                        then
                          (Printf.printf
                             "read the shifted values into shift buffer...\n%!";
                           (let shift = bytes_shifted * 8
                            in
                              (* assert <= 16 *)
                              (assert (shift <= 16);
                               for i = 0 to num_samples - 1 do
                                 Bigarray.Array1.set !shift_buffer (i * 2)
                                   (BitBuffer.read !shift_bits shift);
                                 Bigarray.Array1.set !shift_buffer
                                   ((i * 2) + 1)
                                   (BitBuffer.read !shift_bits shift)
                               done)))
                        else ();
                        (* un-mix the data and convert to output format *)
                        (* - note that mix_res = 0 means just interleave so we use that path for uncompressed frames *)
                        (*
				out16 = &((int16_t * )sampleBuffer)[channelIndex];
				unmix16 mix_buffer_u mix_buffer_v out16 num_channels num_samples mix_bits mix_res
			*)
                        let out16 =
                          BigarrayUtils.uint8_to_int16 sample_buffer
                        in
                          (* *out_num_samples = num_samples *)
                          (Printf.printf
                             "un-mix data and convert to output format...\n%!";
                           Matrix.unmix16 !mix_buffer_U !mix_buffer_V out16
                             num_channels num_samples !mix_bits !mix_res;
                           out_num_samples := num_samples;
                           Printf.printf "processed %d samples\n%!"
                             num_samples;
                           Printf.printf "bitbuffer position: %d.%d\n%!"
                             bits.BitBuffer.current bits.BitBuffer.bit_index;
                           to_hex2 (Array1.sub sample_buffer 0 num_samples))))))
          | DSE -> (* data stream element -- parse but ignore *)
              (Printf.printf "data stream element\n%!";
               data_stream_element bits)
          | FIL -> (* fill element -- parse but ignore *)
              (Printf.printf "fill element\n%!"; fill_element bits)
          | END ->
              (Printf.printf "end element; byte aligning bit buffer\n%!";
               BitBuffer.byte_align bits false;
               raise Done)
          | x -> failf "unexpected frame element: %d%!" (of_element x));
         (*done;*)
         !out_num_samples)
    with | Done -> !out_num_samples
  
let openfile filename =
  let (cookie, mdat) = Mp4.openfile filename in
  let cookie = init cookie
  in
    (* set up global config *)
    (* allocate mix buffers *)
    (* allocate dynamic predictor *)
    (* "shift off" buffer shares memory with predictor buffer *)
    (print_specific_config cookie;
     config := cookie;
     mix_buffer_U :=
       Array1.create int32 c_layout ((Int32.to_int cookie.frame_length) * 4);
     mix_buffer_V :=
       Array1.create int32 c_layout ((Int32.to_int cookie.frame_length) * 4);
     predictor :=
       Array1.create int32 c_layout ((Int32.to_int cookie.frame_length) * 4);
     shift_buffer := BigarrayUtils.int32_to_uint16 !predictor;
     mdat)
  
let to_pcm_data filename =
  let mdat = openfile filename in (* a bitstring... *)
  let decode_buffer = Array1.create int8_unsigned c_layout (4096 lsl 6) in
  let bitbuffer = BitBuffer.from_bitstring mdat
  in
    (Printf.printf "media data length = %d bytes\n"
       ((Bitstring.bitstring_length mdat) / 8);
     let i = ref 1 in
     let more = ref true in
     let oc = open_out_bin "pcm.raw"
     in
       (while !more do
          (try
             let decoded =
               decode bitbuffer decode_buffer
                 (Int32.to_int !config.frame_length) !config.num_channels
             in
               (Printf.printf "decoded buffer %d\n%!" !i;
                incr i;
                if
                  bitbuffer.BitBuffer.current >=
                    bitbuffer.BitBuffer.byte_size
                then (Printf.printf "done!\n"; more := false)
                else ();
                (* not terribly efficient... *)
                for i = 0 to
                  (((decoded * !config.num_channels) * !config.bit_depth) / 8)
                    - 1
                  do
                  output_byte oc (Bigarray.Array1.get decode_buffer i)
                done)
           with
           | Failure msg ->
               Printf.printf "WARNING: %s\nskipping frame...\n%!" msg)
          done;
        (*to_hex decode_buffer;*)
        close_out oc))
  
let () = to_pcm_data Sys.argv.(1)
  

