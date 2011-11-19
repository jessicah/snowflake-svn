(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Gallium, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../../LICENSE.  *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Dynamic loading of .cmx files *)

type handle

external ndl_open: string -> bool -> handle * string = "caml_natdynlink_open"
external ndl_run: handle -> string -> unit = "caml_natdynlink_run"
external ndl_getmap: unit -> string = "caml_natdynlink_getmap"
external ndl_globals_inited: unit -> int = "caml_natdynlink_globals_inited"

(** snowflake version: filename file-contents => soname, [symbol, value; ..] *)
external ndl_load_elf: string -> string -> string * (string * string) list = "caml_elfopen"

external ndl_register_frametable: string -> unit = "caml_natdynlink_register_frametable"
external ndl_register_global: string -> unit = "caml_natdynlink_register_global"
external ndl_segment: string -> string -> bool -> unit = "caml_natdynlink_segment"
external ndl_execute: string -> unit = "caml_natdynlink_execute"

type linking_error =
    Undefined_global of string
  | Unavailable_primitive of string
  | Uninitialized_global of string

type error =
    Not_a_bytecode_file of string
  | Inconsistent_import of string
  | Unavailable_unit of string
  | Unsafe_file
  | Linking_error of string * linking_error
  | Corrupted_interface of string
  | File_not_found of string
  | Cannot_open_dll of string
  | Inconsistent_implementation of string

exception Error of error

(* Copied from other places to avoid dependencies *)

type dynunit = {
  name: string;
  crc: Digest.t;
  imports_cmi: (string * Digest.t) list;
  imports_cmx: (string * Digest.t) list;
  defines: string list;
}

type dynheader = {
  magic: string;
  units: dynunit list;
}

let dyn_magic_number = "Caml2007D001"

let read_file filename priv =
	(* modified for snowflake... should be interesting... *)
	let ic = open_in_bin filename in
	let buffer = Buffer.create (1024 * 1042) in
	begin try
		while true do
			Buffer.add_char buffer (input_char ic);
		done;
	with End_of_file -> () end;
	let soname, symbols = ndl_load_elf filename (Buffer.contents buffer) in
	
	(* find "caml_plugin_header" symbol *)
	let symbol = begin try 
			List.assoc "caml_plugin_header" symbols
		with _ -> raise(Error(Cannot_open_dll "not an OCaml plugin"))
	end in (* : string *)
	
	(* sym is actually void* which is here treated as a string *)
	let header : dynheader = Marshal.from_string symbol 0 in
	if header.magic <> dyn_magic_number
	then raise(Error(Not_a_bytecode_file filename));
	(* what to return...? *)
	(soname, symbols, header.units)

let cmx_not_found_crc =
  "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"


(* Management of interface and implementation CRCs *)

module StrMap = Map.Make(String)

type implem_state =
  | Loaded
  | Check_inited of int

type state = {
  ifaces: (string*string) StrMap.t;
  implems: (string*string*implem_state) StrMap.t;
}

let empty_state = {
  ifaces = StrMap.empty;
  implems = StrMap.empty;
}

let global_state = ref empty_state

let allow_extension = ref true

let inited = ref false

let default_available_units () =
  let map : (string*Digest.t*Digest.t*string list) list =
    Marshal.from_string (ndl_getmap ()) 0 in
  let exe = Sys.executable_name in
  let rank = ref 0 in
  global_state :=
    List.fold_left
      (fun st (name,crc_intf,crc_impl,syms) ->
        rank := !rank + List.length syms;
        {
         ifaces = StrMap.add name (crc_intf,exe) st.ifaces;
         implems = StrMap.add name (crc_impl,exe,Check_inited !rank) st.implems;
        }
      )
      empty_state
      map;
  allow_extension := true;
  inited := true

let init () =
  if not !inited then default_available_units ()

let add_check_ifaces allow_ext filename ui ifaces =
  List.fold_left
    (fun ifaces (name, crc) ->
       if name = ui.name
       then StrMap.add name (crc,filename) ifaces
       else
         try
           let (old_crc,old_src) = StrMap.find name ifaces in
           if old_crc <> crc
           then raise(Error(Inconsistent_import(name)))
           else ifaces
         with Not_found ->
           if allow_ext then StrMap.add name (crc,filename) ifaces
           else raise (Error(Unavailable_unit name))
    ) ifaces ui.imports_cmi

let check_implems filename ui implems =
  List.iter
    (fun (name, crc) ->
       match name with
         |"Out_of_memory"
         |"Sys_error"
         |"Failure"
         |"Invalid_argument"
         |"End_of_file"
         |"Division_by_zero"
         |"Not_found"
         |"Match_failure"
         |"Stack_overflow"
         |"Sys_blocked_io"
         |"Assert_failure"
         |"Undefined_recursive_module" -> ()
         | _ ->
       try
         let (old_crc,old_src,state) = StrMap.find name implems in
         if crc <> cmx_not_found_crc && old_crc <> crc
         then raise(Error(Inconsistent_implementation(name)))
         else match state with
           | Check_inited i ->
               if ndl_globals_inited() < i
               then raise(Error(Unavailable_unit name))
           | Loaded -> ()
       with Not_found ->
         raise (Error(Unavailable_unit name))
    ) ui.imports_cmx

let optsym unit name symbols =
	let sym = Printf.sprintf "caml%s%s" unit name in
	begin try
		Some (List.assoc sym symbols)
	with Not_found -> None
	end

let may f = function
	| None -> ()
	| Some x -> f x
	
let load_symbol symbols symbol =
	let optsym n = optsym symbol n symbols in
	let frametable, unit, data, data_end, code, code_end, entrypoint =
		optsym "__frametable", optsym "",
		optsym "__data_begin", optsym "__data_end",
		optsym "__code_begin", optsym "__code_end",
		optsym "__entry"
	in
	may ndl_register_frametable frametable;
	may ndl_register_global unit;
	begin match data, data_end with
	| Some x, Some y -> ndl_segment x y false
	| _ -> ()
	end;
	begin match code, code_end with
	| Some x, Some y -> ndl_segment x y true
	| _ -> ()
	end;
	may ndl_execute entrypoint

let loadunits filename symbols units state =
  let new_ifaces =
    List.fold_left
      (fun accu ui -> add_check_ifaces !allow_extension filename ui accu)
      state.ifaces units in
  let new_implems =
    List.fold_left
      (fun accu ui ->
         check_implems filename ui accu;
         StrMap.add ui.name (ui.crc,filename,Loaded) accu)
      state.implems units in

  let defines = List.flatten (List.map (fun ui -> ui.defines) units) in

	load_symbol symbols "_shared_startup";
	List.iter (load_symbol symbols) defines;
  (*ndl_run handle "_shared_startup";
  List.iter (ndl_run handle) defines;*)
  { implems = new_implems; ifaces = new_ifaces }

let load priv filename =
  init();
  let (filename,symbols,units) = read_file filename priv in
  let nstate = loadunits filename symbols units !global_state in
  if not priv then global_state := nstate

let loadfile filename = load false filename
let loadfile_private filename = load true filename

let allow_only names =
  init();
  let old = !global_state.ifaces in
  let ifaces =
    List.fold_left
      (fun ifaces name ->
         try StrMap.add name (StrMap.find name old) ifaces
         with Not_found -> ifaces)
      StrMap.empty names in
  global_state := { !global_state with ifaces = ifaces };
  allow_extension := false

let prohibit names =
  init();
  let ifaces = List.fold_right StrMap.remove names !global_state.ifaces in
  global_state := { !global_state with ifaces = ifaces };
  allow_extension := false

let allow_unsafe_modules _ =
  ()

(* Error report *)

let error_message = function
    Not_a_bytecode_file name ->
      name ^ " is not an object file"
  | Inconsistent_import name ->
      "interface mismatch on " ^ name
  | Unavailable_unit name ->
      "no implementation available for " ^ name
  | Unsafe_file ->
      "this object file uses unsafe features"
  | Linking_error (name, Undefined_global s) ->
      "error while linking " ^ name ^ ".\n" ^
      "Reference to undefined global `" ^ s ^ "'"
  | Linking_error (name, Unavailable_primitive s) ->
      "error while linking " ^ name ^ ".\n" ^
      "The external function `" ^ s ^ "' is not available"
  | Linking_error (name, Uninitialized_global s) ->
      "error while linking " ^ name ^ ".\n" ^
      "The module `" ^ s ^ "' is not yet initialized"
  | Corrupted_interface name ->
      "corrupted interface file " ^ name
  | File_not_found name ->
      "cannot find file " ^ name ^ " in search path"
  | Cannot_open_dll reason ->
      "error loading shared library: " ^ reason
  | Inconsistent_implementation name ->
      "implementation mismatch on " ^ name

let is_native = true
(*let adapt_filename f = Filename.chop_extension f ^ ".cmxs"*)
