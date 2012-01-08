(* MP4 : Parsing the container format *)
open Bitstring
  
let box bits =
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
                 | 0x0000l when true ->
                     if __pabitstring_len_1003 >= 32
                     then
                       (let str =
                          if __pabitstring_off_aligned_1006
                          then
                            (let o =
                               (__pabitstring_original_off_1004 lsr 3) + 4
                             in String.sub __pabitstring_data_1001 o 4)
                          else
                            Bitstring.string_of_bitstring
                              (__pabitstring_data_1001,
                               __pabitstring_off_1002, 32) in
                        let __pabitstring_off_1002 =
                          __pabitstring_off_1002 + 32
                        and __pabitstring_len_1003 =
                          __pabitstring_len_1003 - 32
                        in
                          match str with
                          | kind when true ->
                              if __pabitstring_len_1003 >= 64
                              then
                                (let v =
                                   if __pabitstring_off_aligned_1006
                                   then
                                     (let o =
                                        (__pabitstring_original_off_1004 lsr
                                           3)
                                          + 8 in
                                      let zero = Int64.of_int 0
                                      in
                                        Bitstring.
                                          extract_fastpath_int64_be_unsigned
                                          __pabitstring_data_1001 o zero)
                                   else
                                     Bitstring.extract_int64_be_unsigned
                                       __pabitstring_data_1001
                                       __pabitstring_off_1002
                                       __pabitstring_len_1003 64 in
                                 let __pabitstring_off_1002 =
                                   __pabitstring_off_1002 + 64
                                 and __pabitstring_len_1003 =
                                   __pabitstring_len_1003 - 64
                                 in
                                   match v with
                                   | size when true ->
                                       if
                                         ((((Int64.to_int size) * 8) - 128)
                                            >= 0)
                                           &&
                                           ((((Int64.to_int size) * 8) - 128)
                                              <= __pabitstring_len_1003)
                                       then
                                         (let data =
                                            (__pabitstring_data_1001,
                                             __pabitstring_off_1002,
                                             (((Int64.to_int size) * 8) - 128)) in
                                          let __pabitstring_off_1002 
                                            =
                                            __pabitstring_off_1002 +
                                              (((Int64.to_int size) * 8) -
                                                 128)
                                          and __pabitstring_len_1003 
                                            =
                                            __pabitstring_len_1003 -
                                              (((Int64.to_int size) * 8) -
                                                 128) in
                                          let rest =
                                            (__pabitstring_data_1001,
                                             __pabitstring_off_1002,
                                             __pabitstring_len_1003) in
                                          let __pabitstring_off_1002 
                                            =
                                            __pabitstring_off_1002 +
                                              __pabitstring_len_1003 in
                                          let __pabitstring_len_1003 = 0
                                          in
                                            (__pabitstring_result_1007 :=
                                               Some
                                                 ((* a large box; the Int64.to_int defies purpose of a large box... *)
                                                 (kind, data, rest));
                                             raise Exit))
                                       else ()
                                   | _ -> ())
                              else ()
                          | _ -> ())
                     else ()
                 | _ -> ())
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
                 | 0x0000l when true ->
                     if __pabitstring_len_1003 >= 32
                     then
                       (let str =
                          if __pabitstring_off_aligned_1006
                          then
                            (let o =
                               (__pabitstring_original_off_1004 lsr 3) + 4
                             in String.sub __pabitstring_data_1001 o 4)
                          else
                            Bitstring.string_of_bitstring
                              (__pabitstring_data_1001,
                               __pabitstring_off_1002, 32) in
                        let __pabitstring_off_1002 =
                          __pabitstring_off_1002 + 32
                        and __pabitstring_len_1003 =
                          __pabitstring_len_1003 - 32
                        in
                          match str with
                          | kind when true ->
                              let data =
                                (__pabitstring_data_1001,
                                 __pabitstring_off_1002,
                                 __pabitstring_len_1003) in
                              let __pabitstring_off_1002 =
                                __pabitstring_off_1002 +
                                  __pabitstring_len_1003 in
                              let __pabitstring_len_1003 = 0
                              in
                                (__pabitstring_result_1007 :=
                                   Some ((* consumes remainder of the file *)
                                     (kind, data, empty_bitstring));
                                 raise Exit)
                          | _ -> ())
                     else ()
                 | _ -> ())
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
                 | size when true ->
                     if __pabitstring_len_1003 >= 32
                     then
                       (let str =
                          if __pabitstring_off_aligned_1006
                          then
                            (let o =
                               (__pabitstring_original_off_1004 lsr 3) + 4
                             in String.sub __pabitstring_data_1001 o 4)
                          else
                            Bitstring.string_of_bitstring
                              (__pabitstring_data_1001,
                               __pabitstring_off_1002, 32) in
                        let __pabitstring_off_1002 =
                          __pabitstring_off_1002 + 32
                        and __pabitstring_len_1003 =
                          __pabitstring_len_1003 - 32
                        in
                          match str with
                          | kind when true ->
                              if
                                ((((Int32.to_int size) * 8) - 64) >= 0) &&
                                  ((((Int32.to_int size) * 8) - 64) <=
                                     __pabitstring_len_1003)
                              then
                                (let data =
                                   (__pabitstring_data_1001,
                                    __pabitstring_off_1002,
                                    (((Int32.to_int size) * 8) - 64)) in
                                 let __pabitstring_off_1002 =
                                   __pabitstring_off_1002 +
                                     (((Int32.to_int size) * 8) - 64)
                                 and __pabitstring_len_1003 =
                                   __pabitstring_len_1003 -
                                     (((Int32.to_int size) * 8) - 64) in
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
                                      Some ((* a regular box *)
                                        (kind, data, rest));
                                    raise Exit))
                              else ()
                          | _ -> ())
                     else ()
                 | _ -> ())
            else ();
            __pabitstring_result_1007 :=
              Some (failwith "mp4: failure parsing box");
            raise Exit)
         with | Exit -> ());
        match !__pabitstring_result_1007 with
        | Some x -> x
        | None -> raise (Match_failure ("mp4.mlp", 6, 15))))
  
let fullbox bits =
  let (kind, data, rest) = box bits in
  let (__pabitstring_data_1008, __pabitstring_original_off_1011,
       __pabitstring_original_len_1012) =
    data in
  let __pabitstring_off_1009 = __pabitstring_original_off_1011
  and __pabitstring_len_1010 = __pabitstring_original_len_1012 in
  let __pabitstring_off_aligned_1013 = (__pabitstring_off_1009 land 7) = 0
  in
    (ignore __pabitstring_off_aligned_1013;
     let __pabitstring_result_1014 = ref None
     in
       ((try
           (if __pabitstring_len_1010 >= 8
            then
              (let v =
                 if __pabitstring_off_aligned_1013
                 then
                   (let o = (__pabitstring_original_off_1011 lsr 3) + 0
                    in
                      Char.code (String.unsafe_get __pabitstring_data_1008 o))
                 else
                   Bitstring.extract_char_unsigned __pabitstring_data_1008
                     __pabitstring_off_1009 __pabitstring_len_1010 8 in
               let __pabitstring_off_1009 = __pabitstring_off_1009 + 8
               and __pabitstring_len_1010 = __pabitstring_len_1010 - 8
               in
                 match v with
                 | version when true ->
                     if __pabitstring_len_1010 >= 24
                     then
                       (let __pabitstring_val_1015 =
                          Bitstring.extract_int_be_unsigned
                            __pabitstring_data_1008 __pabitstring_off_1009
                            __pabitstring_len_1010 24 in
                        let __pabitstring_off_1009 =
                          __pabitstring_off_1009 + 24
                        and __pabitstring_len_1010 =
                          __pabitstring_len_1010 - 24
                        in
                          match __pabitstring_val_1015 with
                          | flags when true ->
                              let data' =
                                (__pabitstring_data_1008,
                                 __pabitstring_off_1009,
                                 __pabitstring_len_1010) in
                              let __pabitstring_off_1009 =
                                __pabitstring_off_1009 +
                                  __pabitstring_len_1010 in
                              let __pabitstring_len_1010 = 0
                              in
                                (__pabitstring_result_1014 :=
                                   Some
                                     ((* data is actually version : flags : data *)
                                     (kind, data', version, flags, rest));
                                 raise Exit)
                          | _ -> ())
                     else ()
                 | _ -> ())
            else ();
            __pabitstring_result_1014 :=
              Some (failwith "mp4: failure parsing full box");
            raise Exit)
         with | Exit -> ());
        match !__pabitstring_result_1014 with
        | Some x -> x
        | None -> raise (Match_failure ("mp4.mlp", 29, 1))))
  
let rec boxes bits acc =
  if (bitstring_length bits) = 0
  then List.rev acc
  else
    (let (kind, data, rest) = box bits in boxes rest ((kind, data) :: acc))
  
let rec find_box kind parent =
  let children = try boxes parent [] with | Failure _ -> [] in
  let rec loop =
    function
    | [] -> None
    | (k, data) :: _ when k = kind -> Some data
    | (_, data) :: xs ->
        let r = find_box kind data in if r = None then loop xs else r
  in loop children
  
let find_box kind parent =
  match find_box kind parent with
  | None -> failwith "mp4: unable to find box"
  | Some box -> box
  
(* the proper way to find a nested box rather than find_box above *)
let rec get_box path bits =
  match path with
  | [] -> bits
  | k :: kinds ->
      let children = boxes bits [] in get_box kinds (List.assoc k children)
  
let show_sample_description bits =
  let (__pabitstring_data_1016, __pabitstring_original_off_1019,
       __pabitstring_original_len_1020) =
    bits in
  let __pabitstring_off_1017 = __pabitstring_original_off_1019
  and __pabitstring_len_1018 = __pabitstring_original_len_1020 in
  let __pabitstring_off_aligned_1021 = (__pabitstring_off_1017 land 7) = 0
  in
    (ignore __pabitstring_off_aligned_1021;
     let __pabitstring_result_1022 = ref None
     in
       ((try
           (if __pabitstring_len_1018 >= 32
            then
              (let v =
                 if __pabitstring_off_aligned_1021
                 then
                   (let o = (__pabitstring_original_off_1019 lsr 3) + 0 in
                    let zero = Int32.of_int 0
                    in
                      Bitstring.extract_fastpath_int32_be_unsigned
                        __pabitstring_data_1016 o zero)
                 else
                   Bitstring.extract_int32_be_unsigned
                     __pabitstring_data_1016 __pabitstring_off_1017
                     __pabitstring_len_1018 32 in
               let __pabitstring_off_1017 = __pabitstring_off_1017 + 32
               and __pabitstring_len_1018 = __pabitstring_len_1018 - 32
               in
                 match v with
                 | _ when true -> (* version & flags *)
                     if __pabitstring_len_1018 >= 32
                     then
                       (let v =
                          if __pabitstring_off_aligned_1021
                          then
                            (let o =
                               (__pabitstring_original_off_1019 lsr 3) + 4 in
                             let zero = Int32.of_int 0
                             in
                               Bitstring.extract_fastpath_int32_be_unsigned
                                 __pabitstring_data_1016 o zero)
                          else
                            Bitstring.extract_int32_be_unsigned
                              __pabitstring_data_1016 __pabitstring_off_1017
                              __pabitstring_len_1018 32 in
                        let __pabitstring_off_1017 =
                          __pabitstring_off_1017 + 32
                        and __pabitstring_len_1018 =
                          __pabitstring_len_1018 - 32
                        in
                          match v with
                          | 0x1l when true -> (* num_entries *)
                              if __pabitstring_len_1018 >= 32
                              then
                                (let v =
                                   if __pabitstring_off_aligned_1021
                                   then
                                     (let o =
                                        (__pabitstring_original_off_1019 lsr
                                           3)
                                          + 8 in
                                      let zero = Int32.of_int 0
                                      in
                                        Bitstring.
                                          extract_fastpath_int32_be_unsigned
                                          __pabitstring_data_1016 o zero)
                                   else
                                     Bitstring.extract_int32_be_unsigned
                                       __pabitstring_data_1016
                                       __pabitstring_off_1017
                                       __pabitstring_len_1018 32 in
                                 let __pabitstring_off_1017 =
                                   __pabitstring_off_1017 + 32
                                 and __pabitstring_len_1018 =
                                   __pabitstring_len_1018 - 32
                                 in
                                   match v with
                                   | _ when true -> (* size *)
                                       if __pabitstring_len_1018 >= 32
                                       then
                                         (let str =
                                            if __pabitstring_off_aligned_1021
                                            then
                                              (let o =
                                                 (__pabitstring_original_off_1019
                                                    lsr 3)
                                                   + 12
                                               in
                                                 String.sub
                                                   __pabitstring_data_1016 o
                                                   4)
                                            else
                                              Bitstring.string_of_bitstring
                                                (__pabitstring_data_1016,
                                                 __pabitstring_off_1017, 32) in
                                          let __pabitstring_off_1017 
                                            = __pabitstring_off_1017 + 32
                                          and __pabitstring_len_1018 
                                            = __pabitstring_len_1018 - 32
                                          in
                                            match str with
                                            | format_id when true ->
                                                if
                                                  __pabitstring_len_1018 >=
                                                    48
                                                then
                                                  (let __pabitstring_val_1023 
                                                     =
                                                     Bitstring.
                                                       extract_int64_be_unsigned
                                                       __pabitstring_data_1016
                                                       __pabitstring_off_1017
                                                       __pabitstring_len_1018
                                                       48 in
                                                   let __pabitstring_off_1017 
                                                     =
                                                     __pabitstring_off_1017 +
                                                       48
                                                   and
                                                     __pabitstring_len_1018 
                                                     =
                                                     __pabitstring_len_1018 -
                                                       48
                                                   in
                                                     match __pabitstring_val_1023
                                                     with
                                                     | _ when true ->
                                                         (* reserved *)
                                                         if
                                                           __pabitstring_len_1018
                                                             >= 16
                                                         then
                                                           (let v =
                                                              if
                                                                __pabitstring_off_aligned_1021
                                                              then
                                                                (let o 
                                                                   =
                                                                   (__pabitstring_original_off_1019
                                                                    lsr 3) +
                                                                    22
                                                                 in
                                                                   Bitstring.
                                                                    extract_fastpath_int16_be_unsigned
                                                                    __pabitstring_data_1016
                                                                    o)
                                                              else
                                                                Bitstring.
                                                                  extract_int_be_unsigned
                                                                  __pabitstring_data_1016
                                                                  __pabitstring_off_1017
                                                                  __pabitstring_len_1018
                                                                  16 in
                                                            let __pabitstring_off_1017 
                                                              =
                                                              __pabitstring_off_1017
                                                                + 16
                                                            and
                                                              __pabitstring_len_1018 
                                                              =
                                                              __pabitstring_len_1018
                                                                - 16
                                                            in
                                                              match v with
                                                              | 0x1 when true
                                                                  ->
                                                                  if
                                                                    __pabitstring_len_1018
                                                                    >= 64
                                                                  then
                                                                    (
                                                                    let v 
                                                                    =
                                                                    if
                                                                    __pabitstring_off_aligned_1021
                                                                    then
                                                                    (let o 
                                                                    =
                                                                    (__pabitstring_original_off_1019
                                                                    lsr 3) +
                                                                    24 in
                                                                    let zero 
                                                                    =
                                                                    Int64.
                                                                    of_int 0
                                                                    in
                                                                    Bitstring.
                                                                    extract_fastpath_int64_be_unsigned
                                                                    __pabitstring_data_1016
                                                                    o zero)
                                                                    else
                                                                    Bitstring.
                                                                    extract_int64_be_unsigned
                                                                    __pabitstring_data_1016
                                                                    __pabitstring_off_1017
                                                                    __pabitstring_len_1018
                                                                    64 in
                                                                    let __pabitstring_off_1017 
                                                                    =
                                                                    __pabitstring_off_1017
                                                                    + 64
                                                                    and
                                                                    __pabitstring_len_1018 
                                                                    =
                                                                    __pabitstring_len_1018
                                                                    - 64
                                                                    in
                                                                    match v
                                                                    with
                                                                    | 
                                                                    _ when
                                                                    true ->
                                                                    (* revision level, vendor, reserved *)
                                                                    if
                                                                    __pabitstring_len_1018
                                                                    >= 16
                                                                    then
                                                                    (let v 
                                                                    =
                                                                    if
                                                                    __pabitstring_off_aligned_1021
                                                                    then
                                                                    (let o 
                                                                    =
                                                                    (__pabitstring_original_off_1019
                                                                    lsr 3) +
                                                                    32
                                                                    in
                                                                    Bitstring.
                                                                    extract_fastpath_int16_be_unsigned
                                                                    __pabitstring_data_1016
                                                                    o)
                                                                    else
                                                                    Bitstring.
                                                                    extract_int_be_unsigned
                                                                    __pabitstring_data_1016
                                                                    __pabitstring_off_1017
                                                                    __pabitstring_len_1018
                                                                    16 in
                                                                    let __pabitstring_off_1017 
                                                                    =
                                                                    __pabitstring_off_1017
                                                                    + 16
                                                                    and
                                                                    __pabitstring_len_1018 
                                                                    =
                                                                    __pabitstring_len_1018
                                                                    - 16
                                                                    in
                                                                    match v
                                                                    with
                                                                    | 
                                                                    channels_per_frame
                                                                    when true
                                                                    ->
                                                                    if
                                                                    __pabitstring_len_1018
                                                                    >= 16
                                                                    then
                                                                    (let v 
                                                                    =
                                                                    if
                                                                    __pabitstring_off_aligned_1021
                                                                    then
                                                                    (let o 
                                                                    =
                                                                    (__pabitstring_original_off_1019
                                                                    lsr 3) +
                                                                    34
                                                                    in
                                                                    Bitstring.
                                                                    extract_fastpath_int16_be_unsigned
                                                                    __pabitstring_data_1016
                                                                    o)
                                                                    else
                                                                    Bitstring.
                                                                    extract_int_be_unsigned
                                                                    __pabitstring_data_1016
                                                                    __pabitstring_off_1017
                                                                    __pabitstring_len_1018
                                                                    16 in
                                                                    let __pabitstring_off_1017 
                                                                    =
                                                                    __pabitstring_off_1017
                                                                    + 16
                                                                    and
                                                                    __pabitstring_len_1018 
                                                                    =
                                                                    __pabitstring_len_1018
                                                                    - 16
                                                                    in
                                                                    match v
                                                                    with
                                                                    | 
                                                                    bits_per_channel
                                                                    when true
                                                                    ->
                                                                    if
                                                                    __pabitstring_len_1018
                                                                    >= 32
                                                                    then
                                                                    (let v 
                                                                    =
                                                                    if
                                                                    __pabitstring_off_aligned_1021
                                                                    then
                                                                    (let o 
                                                                    =
                                                                    (__pabitstring_original_off_1019
                                                                    lsr 3) +
                                                                    36 in
                                                                    let zero 
                                                                    =
                                                                    Int32.
                                                                    of_int 0
                                                                    in
                                                                    Bitstring.
                                                                    extract_fastpath_int32_be_unsigned
                                                                    __pabitstring_data_1016
                                                                    o zero)
                                                                    else
                                                                    Bitstring.
                                                                    extract_int32_be_unsigned
                                                                    __pabitstring_data_1016
                                                                    __pabitstring_off_1017
                                                                    __pabitstring_len_1018
                                                                    32 in
                                                                    let __pabitstring_off_1017 
                                                                    =
                                                                    __pabitstring_off_1017
                                                                    + 32
                                                                    and
                                                                    __pabitstring_len_1018 
                                                                    =
                                                                    __pabitstring_len_1018
                                                                    - 32
                                                                    in
                                                                    match v
                                                                    with
                                                                    | 
                                                                    _ when
                                                                    true ->
                                                                    (* compression id, packet size *)
                                                                    if
                                                                    __pabitstring_len_1018
                                                                    >= 16
                                                                    then
                                                                    (let v 
                                                                    =
                                                                    if
                                                                    __pabitstring_off_aligned_1021
                                                                    then
                                                                    (let o 
                                                                    =
                                                                    (__pabitstring_original_off_1019
                                                                    lsr 3) +
                                                                    40
                                                                    in
                                                                    Bitstring.
                                                                    extract_fastpath_int16_be_unsigned
                                                                    __pabitstring_data_1016
                                                                    o)
                                                                    else
                                                                    Bitstring.
                                                                    extract_int_be_unsigned
                                                                    __pabitstring_data_1016
                                                                    __pabitstring_off_1017
                                                                    __pabitstring_len_1018
                                                                    16 in
                                                                    let __pabitstring_off_1017 
                                                                    =
                                                                    __pabitstring_off_1017
                                                                    + 16
                                                                    and
                                                                    __pabitstring_len_1018 
                                                                    =
                                                                    __pabitstring_len_1018
                                                                    - 16
                                                                    in
                                                                    match v
                                                                    with
                                                                    | 
                                                                    sample_rate
                                                                    when true
                                                                    ->
                                                                    if
                                                                    __pabitstring_len_1018
                                                                    >= 16
                                                                    then
                                                                    (let v 
                                                                    =
                                                                    if
                                                                    __pabitstring_off_aligned_1021
                                                                    then
                                                                    (let o 
                                                                    =
                                                                    (__pabitstring_original_off_1019
                                                                    lsr 3) +
                                                                    42
                                                                    in
                                                                    Bitstring.
                                                                    extract_fastpath_int16_be_unsigned
                                                                    __pabitstring_data_1016
                                                                    o)
                                                                    else
                                                                    Bitstring.
                                                                    extract_int_be_unsigned
                                                                    __pabitstring_data_1016
                                                                    __pabitstring_off_1017
                                                                    __pabitstring_len_1018
                                                                    16 in
                                                                    let __pabitstring_off_1017 
                                                                    =
                                                                    __pabitstring_off_1017
                                                                    + 16
                                                                    and
                                                                    __pabitstring_len_1018 
                                                                    =
                                                                    __pabitstring_len_1018
                                                                    - 16
                                                                    in
                                                                    match v
                                                                    with
                                                                    | 
                                                                    _ when
                                                                    true ->
                                                                    let cookie 
                                                                    =
                                                                    (__pabitstring_data_1016,
                                                                    __pabitstring_off_1017,
                                                                    __pabitstring_len_1018) in
                                                                    let __pabitstring_off_1017 
                                                                    =
                                                                    __pabitstring_off_1017
                                                                    +
                                                                    __pabitstring_len_1018 in
                                                                    let __pabitstring_len_1018 
                                                                    = 0
                                                                    in
                                                                    (__pabitstring_result_1022 :=
                                                                    Some
                                                                    (Printf.
                                                                    printf
                                                                    "format id = %s, channels = %d, bitrate = %d, sample rate = %d\n"
                                                                    format_id
                                                                    channels_per_frame
                                                                    bits_per_channel
                                                                    sample_rate);
                                                                    raise
                                                                    Exit)
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
            __pabitstring_result_1022 :=
              Some (Printf.printf "invalid sample description");
            raise Exit)
         with | Exit -> ());
        match !__pabitstring_result_1022 with
        | Some x -> x
        | None -> raise (Match_failure ("mp4.mlp", 62, 1))))
  
let check_sample_description bits =
  let (__pabitstring_data_1024, __pabitstring_original_off_1027,
       __pabitstring_original_len_1028) =
    bits in
  let __pabitstring_off_1025 = __pabitstring_original_off_1027
  and __pabitstring_len_1026 = __pabitstring_original_len_1028 in
  let __pabitstring_off_aligned_1029 = (__pabitstring_off_1025 land 7) = 0
  in
    (ignore __pabitstring_off_aligned_1029;
     let __pabitstring_result_1030 = ref None
     in
       ((try
           (if __pabitstring_len_1026 >= 32
            then
              (let v =
                 if __pabitstring_off_aligned_1029
                 then
                   (let o = (__pabitstring_original_off_1027 lsr 3) + 0 in
                    let zero = Int32.of_int 0
                    in
                      Bitstring.extract_fastpath_int32_be_unsigned
                        __pabitstring_data_1024 o zero)
                 else
                   Bitstring.extract_int32_be_unsigned
                     __pabitstring_data_1024 __pabitstring_off_1025
                     __pabitstring_len_1026 32 in
               let __pabitstring_off_1025 = __pabitstring_off_1025 + 32
               and __pabitstring_len_1026 = __pabitstring_len_1026 - 32
               in
                 match v with
                 | _ when true -> (* version & flags *)
                     if __pabitstring_len_1026 >= 32
                     then
                       (let v =
                          if __pabitstring_off_aligned_1029
                          then
                            (let o =
                               (__pabitstring_original_off_1027 lsr 3) + 4 in
                             let zero = Int32.of_int 0
                             in
                               Bitstring.extract_fastpath_int32_be_unsigned
                                 __pabitstring_data_1024 o zero)
                          else
                            Bitstring.extract_int32_be_unsigned
                              __pabitstring_data_1024 __pabitstring_off_1025
                              __pabitstring_len_1026 32 in
                        let __pabitstring_off_1025 =
                          __pabitstring_off_1025 + 32
                        and __pabitstring_len_1026 =
                          __pabitstring_len_1026 - 32
                        in
                          match v with
                          | 0x1l when true -> (* num_entries *)
                              if __pabitstring_len_1026 >= 32
                              then
                                (let v =
                                   if __pabitstring_off_aligned_1029
                                   then
                                     (let o =
                                        (__pabitstring_original_off_1027 lsr
                                           3)
                                          + 8 in
                                      let zero = Int32.of_int 0
                                      in
                                        Bitstring.
                                          extract_fastpath_int32_be_unsigned
                                          __pabitstring_data_1024 o zero)
                                   else
                                     Bitstring.extract_int32_be_unsigned
                                       __pabitstring_data_1024
                                       __pabitstring_off_1025
                                       __pabitstring_len_1026 32 in
                                 let __pabitstring_off_1025 =
                                   __pabitstring_off_1025 + 32
                                 and __pabitstring_len_1026 =
                                   __pabitstring_len_1026 - 32
                                 in
                                   match v with
                                   | _ when true -> (* size *)
                                       if __pabitstring_len_1026 >= 32
                                       then
                                         (let str =
                                            if __pabitstring_off_aligned_1029
                                            then
                                              (let o =
                                                 (__pabitstring_original_off_1027
                                                    lsr 3)
                                                   + 12
                                               in
                                                 String.sub
                                                   __pabitstring_data_1024 o
                                                   4)
                                            else
                                              Bitstring.string_of_bitstring
                                                (__pabitstring_data_1024,
                                                 __pabitstring_off_1025, 32) in
                                          let __pabitstring_off_1025 
                                            = __pabitstring_off_1025 + 32
                                          and __pabitstring_len_1026 
                                            = __pabitstring_len_1026 - 32
                                          in
                                            match str with
                                            | format_id when true ->
                                                if
                                                  __pabitstring_len_1026 >=
                                                    48
                                                then
                                                  (let __pabitstring_val_1031 
                                                     =
                                                     Bitstring.
                                                       extract_int64_be_unsigned
                                                       __pabitstring_data_1024
                                                       __pabitstring_off_1025
                                                       __pabitstring_len_1026
                                                       48 in
                                                   let __pabitstring_off_1025 
                                                     =
                                                     __pabitstring_off_1025 +
                                                       48
                                                   and
                                                     __pabitstring_len_1026 
                                                     =
                                                     __pabitstring_len_1026 -
                                                       48
                                                   in
                                                     match __pabitstring_val_1031
                                                     with
                                                     | _ when true ->
                                                         (* reserved *)
                                                         if
                                                           __pabitstring_len_1026
                                                             >= 16
                                                         then
                                                           (let v =
                                                              if
                                                                __pabitstring_off_aligned_1029
                                                              then
                                                                (let o 
                                                                   =
                                                                   (__pabitstring_original_off_1027
                                                                    lsr 3) +
                                                                    22
                                                                 in
                                                                   Bitstring.
                                                                    extract_fastpath_int16_be_unsigned
                                                                    __pabitstring_data_1024
                                                                    o)
                                                              else
                                                                Bitstring.
                                                                  extract_int_be_unsigned
                                                                  __pabitstring_data_1024
                                                                  __pabitstring_off_1025
                                                                  __pabitstring_len_1026
                                                                  16 in
                                                            let __pabitstring_off_1025 
                                                              =
                                                              __pabitstring_off_1025
                                                                + 16
                                                            and
                                                              __pabitstring_len_1026 
                                                              =
                                                              __pabitstring_len_1026
                                                                - 16
                                                            in
                                                              match v with
                                                              | 0x1 when true
                                                                  ->
                                                                  if
                                                                    __pabitstring_len_1026
                                                                    >= 64
                                                                  then
                                                                    (
                                                                    let v 
                                                                    =
                                                                    if
                                                                    __pabitstring_off_aligned_1029
                                                                    then
                                                                    (let o 
                                                                    =
                                                                    (__pabitstring_original_off_1027
                                                                    lsr 3) +
                                                                    24 in
                                                                    let zero 
                                                                    =
                                                                    Int64.
                                                                    of_int 0
                                                                    in
                                                                    Bitstring.
                                                                    extract_fastpath_int64_be_unsigned
                                                                    __pabitstring_data_1024
                                                                    o zero)
                                                                    else
                                                                    Bitstring.
                                                                    extract_int64_be_unsigned
                                                                    __pabitstring_data_1024
                                                                    __pabitstring_off_1025
                                                                    __pabitstring_len_1026
                                                                    64 in
                                                                    let __pabitstring_off_1025 
                                                                    =
                                                                    __pabitstring_off_1025
                                                                    + 64
                                                                    and
                                                                    __pabitstring_len_1026 
                                                                    =
                                                                    __pabitstring_len_1026
                                                                    - 64
                                                                    in
                                                                    match v
                                                                    with
                                                                    | 
                                                                    _ when
                                                                    true ->
                                                                    (* revision level, vendor, reserved *)
                                                                    if
                                                                    __pabitstring_len_1026
                                                                    >= 16
                                                                    then
                                                                    (let v 
                                                                    =
                                                                    if
                                                                    __pabitstring_off_aligned_1029
                                                                    then
                                                                    (let o 
                                                                    =
                                                                    (__pabitstring_original_off_1027
                                                                    lsr 3) +
                                                                    32
                                                                    in
                                                                    Bitstring.
                                                                    extract_fastpath_int16_be_unsigned
                                                                    __pabitstring_data_1024
                                                                    o)
                                                                    else
                                                                    Bitstring.
                                                                    extract_int_be_unsigned
                                                                    __pabitstring_data_1024
                                                                    __pabitstring_off_1025
                                                                    __pabitstring_len_1026
                                                                    16 in
                                                                    let __pabitstring_off_1025 
                                                                    =
                                                                    __pabitstring_off_1025
                                                                    + 16
                                                                    and
                                                                    __pabitstring_len_1026 
                                                                    =
                                                                    __pabitstring_len_1026
                                                                    - 16
                                                                    in
                                                                    match v
                                                                    with
                                                                    | 
                                                                    num_channels
                                                                    when true
                                                                    ->
                                                                    if
                                                                    __pabitstring_len_1026
                                                                    >= 16
                                                                    then
                                                                    (let v 
                                                                    =
                                                                    if
                                                                    __pabitstring_off_aligned_1029
                                                                    then
                                                                    (let o 
                                                                    =
                                                                    (__pabitstring_original_off_1027
                                                                    lsr 3) +
                                                                    34
                                                                    in
                                                                    Bitstring.
                                                                    extract_fastpath_int16_be_unsigned
                                                                    __pabitstring_data_1024
                                                                    o)
                                                                    else
                                                                    Bitstring.
                                                                    extract_int_be_unsigned
                                                                    __pabitstring_data_1024
                                                                    __pabitstring_off_1025
                                                                    __pabitstring_len_1026
                                                                    16 in
                                                                    let __pabitstring_off_1025 
                                                                    =
                                                                    __pabitstring_off_1025
                                                                    + 16
                                                                    and
                                                                    __pabitstring_len_1026 
                                                                    =
                                                                    __pabitstring_len_1026
                                                                    - 16
                                                                    in
                                                                    match v
                                                                    with
                                                                    | 
                                                                    bits_per_channel
                                                                    when true
                                                                    ->
                                                                    if
                                                                    __pabitstring_len_1026
                                                                    >= 32
                                                                    then
                                                                    (let v 
                                                                    =
                                                                    if
                                                                    __pabitstring_off_aligned_1029
                                                                    then
                                                                    (let o 
                                                                    =
                                                                    (__pabitstring_original_off_1027
                                                                    lsr 3) +
                                                                    36 in
                                                                    let zero 
                                                                    =
                                                                    Int32.
                                                                    of_int 0
                                                                    in
                                                                    Bitstring.
                                                                    extract_fastpath_int32_be_unsigned
                                                                    __pabitstring_data_1024
                                                                    o zero)
                                                                    else
                                                                    Bitstring.
                                                                    extract_int32_be_unsigned
                                                                    __pabitstring_data_1024
                                                                    __pabitstring_off_1025
                                                                    __pabitstring_len_1026
                                                                    32 in
                                                                    let __pabitstring_off_1025 
                                                                    =
                                                                    __pabitstring_off_1025
                                                                    + 32
                                                                    and
                                                                    __pabitstring_len_1026 
                                                                    =
                                                                    __pabitstring_len_1026
                                                                    - 32
                                                                    in
                                                                    match v
                                                                    with
                                                                    | 
                                                                    _ when
                                                                    true ->
                                                                    (* compression id, packet size *)
                                                                    if
                                                                    __pabitstring_len_1026
                                                                    >= 16
                                                                    then
                                                                    (let v 
                                                                    =
                                                                    if
                                                                    __pabitstring_off_aligned_1029
                                                                    then
                                                                    (let o 
                                                                    =
                                                                    (__pabitstring_original_off_1027
                                                                    lsr 3) +
                                                                    40
                                                                    in
                                                                    Bitstring.
                                                                    extract_fastpath_int16_be_unsigned
                                                                    __pabitstring_data_1024
                                                                    o)
                                                                    else
                                                                    Bitstring.
                                                                    extract_int_be_unsigned
                                                                    __pabitstring_data_1024
                                                                    __pabitstring_off_1025
                                                                    __pabitstring_len_1026
                                                                    16 in
                                                                    let __pabitstring_off_1025 
                                                                    =
                                                                    __pabitstring_off_1025
                                                                    + 16
                                                                    and
                                                                    __pabitstring_len_1026 
                                                                    =
                                                                    __pabitstring_len_1026
                                                                    - 16
                                                                    in
                                                                    match v
                                                                    with
                                                                    | 
                                                                    sample_rate
                                                                    when true
                                                                    ->
                                                                    if
                                                                    __pabitstring_len_1026
                                                                    >= 16
                                                                    then
                                                                    (let v 
                                                                    =
                                                                    if
                                                                    __pabitstring_off_aligned_1029
                                                                    then
                                                                    (let o 
                                                                    =
                                                                    (__pabitstring_original_off_1027
                                                                    lsr 3) +
                                                                    42
                                                                    in
                                                                    Bitstring.
                                                                    extract_fastpath_int16_be_unsigned
                                                                    __pabitstring_data_1024
                                                                    o)
                                                                    else
                                                                    Bitstring.
                                                                    extract_int_be_unsigned
                                                                    __pabitstring_data_1024
                                                                    __pabitstring_off_1025
                                                                    __pabitstring_len_1026
                                                                    16 in
                                                                    let __pabitstring_off_1025 
                                                                    =
                                                                    __pabitstring_off_1025
                                                                    + 16
                                                                    and
                                                                    __pabitstring_len_1026 
                                                                    =
                                                                    __pabitstring_len_1026
                                                                    - 16
                                                                    in
                                                                    match v
                                                                    with
                                                                    | 
                                                                    _ when
                                                                    true ->
                                                                    let cookie 
                                                                    =
                                                                    (__pabitstring_data_1024,
                                                                    __pabitstring_off_1025,
                                                                    __pabitstring_len_1026) in
                                                                    let __pabitstring_off_1025 
                                                                    =
                                                                    __pabitstring_off_1025
                                                                    +
                                                                    __pabitstring_len_1026 in
                                                                    let __pabitstring_len_1026 
                                                                    = 0
                                                                    in
                                                                    if
                                                                    (format_id
                                                                    = "alac")
                                                                    &&
                                                                    ((num_channels
                                                                    = 2) &&
                                                                    ((bits_per_channel
                                                                    = 16) &&
                                                                    (sample_rate
                                                                    = 44100)))
                                                                    then
                                                                    (__pabitstring_result_1030 :=
                                                                    Some
                                                                    cookie;
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
            __pabitstring_result_1030 :=
              Some
                (failwith
                   "mp4: not a compatible mp4 file; expect alac, 16-bit stereo @ 44100kHz");
            raise Exit)
         with | Exit -> ());
        match !__pabitstring_result_1030 with
        | Some x -> x
        | None -> raise (Match_failure ("mp4.mlp", 82, 1))))
  
let openfile filename =
  (*
		ftyp => check it's actually "M4A "
		moov.trak.mdia.minf.stbl.stsd => parse specific config from this
		mdat => the actual data
	*)
  match boxes (bitstring_of_file filename) [] with
  | ("ftyp", ftyp) :: ("moov", moov) :: rest when
      (String.sub (string_of_bitstring ftyp) 0 4) = "M4A " ->
      (*let stsd = find_box "stsd" moov in*)
      let stsd = get_box [ "trak"; "mdia"; "minf"; "stbl"; "stsd" ] moov in
      let mdat = List.assoc "mdat" rest
      in ((check_sample_description stsd), mdat)
  | _ -> failwith "mp4: unable to parse as an alac media file"
  
(* this stuff is useful for debugging layout of mp4 container *)
type action = | Recurse | Display of (bitstring -> unit)

open Hashtbl
  
let actions = Hashtbl.create 10
  
let () = (* register some actions for some boxes *)
  (add actions "moov" Recurse;
   add actions "trak" Recurse;
   add actions "udta" Recurse;
   add actions "mdia" Recurse;
   add actions "minf" Recurse;
   add actions "stbl" Recurse;
   add actions "stsd" (Display show_sample_description))
  
let rec box indent bits =
  let (__pabitstring_data_1032, __pabitstring_original_off_1035,
       __pabitstring_original_len_1036) =
    bits in
  let __pabitstring_off_1033 = __pabitstring_original_off_1035
  and __pabitstring_len_1034 = __pabitstring_original_len_1036 in
  let __pabitstring_off_aligned_1037 = (__pabitstring_off_1033 land 7) = 0
  in
    (ignore __pabitstring_off_aligned_1037;
     let __pabitstring_result_1038 = ref None
     in
       ((try
           (if __pabitstring_len_1034 >= 32
            then
              (let v =
                 if __pabitstring_off_aligned_1037
                 then
                   (let o = (__pabitstring_original_off_1035 lsr 3) + 0 in
                    let zero = Int32.of_int 0
                    in
                      Bitstring.extract_fastpath_int32_be_unsigned
                        __pabitstring_data_1032 o zero)
                 else
                   Bitstring.extract_int32_be_unsigned
                     __pabitstring_data_1032 __pabitstring_off_1033
                     __pabitstring_len_1034 32 in
               let __pabitstring_off_1033 = __pabitstring_off_1033 + 32
               and __pabitstring_len_1034 = __pabitstring_len_1034 - 32
               in
                 match v with
                 | 0x0000_l when true ->
                     if __pabitstring_len_1034 >= 32
                     then
                       (let str =
                          if __pabitstring_off_aligned_1037
                          then
                            (let o =
                               (__pabitstring_original_off_1035 lsr 3) + 4
                             in String.sub __pabitstring_data_1032 o 4)
                          else
                            Bitstring.string_of_bitstring
                              (__pabitstring_data_1032,
                               __pabitstring_off_1033, 32) in
                        let __pabitstring_off_1033 =
                          __pabitstring_off_1033 + 32
                        and __pabitstring_len_1034 =
                          __pabitstring_len_1034 - 32
                        in
                          match str with
                          | kind when true ->
                              let data =
                                (__pabitstring_data_1032,
                                 __pabitstring_off_1033,
                                 __pabitstring_len_1034) in
                              let __pabitstring_off_1033 =
                                __pabitstring_off_1033 +
                                  __pabitstring_len_1034 in
                              let __pabitstring_len_1034 = 0
                              in
                                (__pabitstring_result_1038 :=
                                   Some
                                     (Printf.printf "%*slast box: %s\n"
                                        indent "" kind;
                                      action indent kind data);
                                 raise Exit)
                          | _ -> ())
                     else ()
                 | _ -> ())
            else ();
            if __pabitstring_len_1034 >= 32
            then
              (let v =
                 if __pabitstring_off_aligned_1037
                 then
                   (let o = (__pabitstring_original_off_1035 lsr 3) + 0 in
                    let zero = Int32.of_int 0
                    in
                      Bitstring.extract_fastpath_int32_be_unsigned
                        __pabitstring_data_1032 o zero)
                 else
                   Bitstring.extract_int32_be_unsigned
                     __pabitstring_data_1032 __pabitstring_off_1033
                     __pabitstring_len_1034 32 in
               let __pabitstring_off_1033 = __pabitstring_off_1033 + 32
               and __pabitstring_len_1034 = __pabitstring_len_1034 - 32
               in
                 match v with
                 | 0x0001_l when true ->
                     if __pabitstring_len_1034 >= 32
                     then
                       (let str =
                          if __pabitstring_off_aligned_1037
                          then
                            (let o =
                               (__pabitstring_original_off_1035 lsr 3) + 4
                             in String.sub __pabitstring_data_1032 o 4)
                          else
                            Bitstring.string_of_bitstring
                              (__pabitstring_data_1032,
                               __pabitstring_off_1033, 32) in
                        let __pabitstring_off_1033 =
                          __pabitstring_off_1033 + 32
                        and __pabitstring_len_1034 =
                          __pabitstring_len_1034 - 32
                        in
                          match str with
                          | kind when true ->
                              if __pabitstring_len_1034 >= 64
                              then
                                (let v =
                                   if __pabitstring_off_aligned_1037
                                   then
                                     (let o =
                                        (__pabitstring_original_off_1035 lsr
                                           3)
                                          + 8 in
                                      let zero = Int64.of_int 0
                                      in
                                        Bitstring.
                                          extract_fastpath_int64_be_unsigned
                                          __pabitstring_data_1032 o zero)
                                   else
                                     Bitstring.extract_int64_be_unsigned
                                       __pabitstring_data_1032
                                       __pabitstring_off_1033
                                       __pabitstring_len_1034 64 in
                                 let __pabitstring_off_1033 =
                                   __pabitstring_off_1033 + 64
                                 and __pabitstring_len_1034 =
                                   __pabitstring_len_1034 - 64
                                 in
                                   match v with
                                   | size when true ->
                                       if
                                         ((((Int64.to_int size) * 8) - 96) >=
                                            0)
                                           &&
                                           ((((Int64.to_int size) * 8) - 96)
                                              <= __pabitstring_len_1034)
                                       then
                                         (let data =
                                            (__pabitstring_data_1032,
                                             __pabitstring_off_1033,
                                             (((Int64.to_int size) * 8) - 96)) in
                                          let __pabitstring_off_1033 
                                            =
                                            __pabitstring_off_1033 +
                                              (((Int64.to_int size) * 8) - 96)
                                          and __pabitstring_len_1034 
                                            =
                                            __pabitstring_len_1034 -
                                              (((Int64.to_int size) * 8) - 96) in
                                          let next =
                                            (__pabitstring_data_1032,
                                             __pabitstring_off_1033,
                                             __pabitstring_len_1034) in
                                          let __pabitstring_off_1033 
                                            =
                                            __pabitstring_off_1033 +
                                              __pabitstring_len_1034 in
                                          let __pabitstring_len_1034 = 0
                                          in
                                            (__pabitstring_result_1038 :=
                                               Some
                                                 (Printf.printf
                                                    "%*slarge box: %s, %Ld bytes\n"
                                                    indent "" kind size;
                                                  action indent kind data;
                                                  box indent next);
                                             raise Exit))
                                       else ()
                                   | _ -> ())
                              else ()
                          | _ -> ())
                     else ()
                 | _ -> ())
            else ();
            if __pabitstring_len_1034 >= 32
            then
              (let v =
                 if __pabitstring_off_aligned_1037
                 then
                   (let o = (__pabitstring_original_off_1035 lsr 3) + 0 in
                    let zero = Int32.of_int 0
                    in
                      Bitstring.extract_fastpath_int32_be_unsigned
                        __pabitstring_data_1032 o zero)
                 else
                   Bitstring.extract_int32_be_unsigned
                     __pabitstring_data_1032 __pabitstring_off_1033
                     __pabitstring_len_1034 32 in
               let __pabitstring_off_1033 = __pabitstring_off_1033 + 32
               and __pabitstring_len_1034 = __pabitstring_len_1034 - 32
               in
                 match v with
                 | size when true ->
                     if __pabitstring_len_1034 >= 32
                     then
                       (let str =
                          if __pabitstring_off_aligned_1037
                          then
                            (let o =
                               (__pabitstring_original_off_1035 lsr 3) + 4
                             in String.sub __pabitstring_data_1032 o 4)
                          else
                            Bitstring.string_of_bitstring
                              (__pabitstring_data_1032,
                               __pabitstring_off_1033, 32) in
                        let __pabitstring_off_1033 =
                          __pabitstring_off_1033 + 32
                        and __pabitstring_len_1034 =
                          __pabitstring_len_1034 - 32
                        in
                          match str with
                          | kind when true ->
                              if
                                ((((Int32.to_int size) * 8) - 64) >= 0) &&
                                  ((((Int32.to_int size) * 8) - 64) <=
                                     __pabitstring_len_1034)
                              then
                                (let data =
                                   (__pabitstring_data_1032,
                                    __pabitstring_off_1033,
                                    (((Int32.to_int size) * 8) - 64)) in
                                 let __pabitstring_off_1033 =
                                   __pabitstring_off_1033 +
                                     (((Int32.to_int size) * 8) - 64)
                                 and __pabitstring_len_1034 =
                                   __pabitstring_len_1034 -
                                     (((Int32.to_int size) * 8) - 64) in
                                 let next =
                                   (__pabitstring_data_1032,
                                    __pabitstring_off_1033,
                                    __pabitstring_len_1034) in
                                 let __pabitstring_off_1033 =
                                   __pabitstring_off_1033 +
                                     __pabitstring_len_1034 in
                                 let __pabitstring_len_1034 = 0
                                 in
                                   (__pabitstring_result_1038 :=
                                      Some
                                        (Printf.printf
                                           "%*ssmall box: %s, %ld bytes\n"
                                           indent "" kind size;
                                         action indent kind data;
                                         box indent next);
                                    raise Exit))
                              else ()
                          | _ -> ())
                     else ()
                 | _ -> ())
            else ();
            __pabitstring_result_1038 := Some ();
            raise Exit)
         with | Exit -> ());
        match !__pabitstring_result_1038 with
        | Some x -> x
        | None -> raise (Match_failure ("mp4.mlp", 135, 1))))
and action indent kind bits =
  try
    match Hashtbl.find actions kind with
    | Recurse -> box (indent + 2) bits
    | Display f -> f bits
  with | Not_found -> ()
  
let run filename = box 0 (Bitstring.bitstring_of_file filename)
  

