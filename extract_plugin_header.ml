
(* extract plugin header *)

(* based on natdynlink *)

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
let marshal_magic = "\x84\x95\xA6\xBE"

let data filename =
	let ic = open_in_bin filename in
	let data = IO.read_all (IO.input_channel ic) in
	close_in ic;
	data

(* magic_offset : 28686 *)
(* intext_magic : 28664 *)
let read filename  =
	let data = data filename in
	let magic_offset = ExtString.String.find data dyn_magic_number in
	let marshal_magic = ExtString.String.find data marshal_magic in
	if marshal_magic > magic_offset
		then failwith "Couldn't find start of plugin data";
	let header : dynheader = Marshal.from_string data marshal_magic in
	if header.magic <> dyn_magic_number
	then failwith "not a plugin";
	header.units

let print units =
	List.iter (fun unit ->
		Printf.printf
			"\n%-40s\n  %-40s\n========================\n" unit.name (Digest.to_hex unit.crc);
		Printf.printf "  imports cmi:\n";
		List.iter (fun (name, cmi_crc) ->
			Printf.printf "%24s:  %s\n" name (Digest.to_hex cmi_crc);
		) unit.imports_cmi;
		Printf.printf "  imports cmx:\n";
		List.iter (fun (name, cmx_crc) ->
			Printf.printf "%24s:  %s\n" name (Digest.to_hex cmx_crc);
		) unit.imports_cmx;
		Printf.printf "  defines units:\n";
		List.iter (Printf.printf "\t%s\n") unit.defines;
	) units

(* buh, so much type-dependencies *)

(* asttypes *)

type constant =
    Const_int of int
  | Const_char of char
  | Const_string of string
  | Const_float of string
  | Const_int32 of int32
  | Const_int64 of int64
  | Const_nativeint of nativeint

type rec_flag = Nonrecursive | Recursive | Default

type direction_flag = Upto | Downto

type private_flag = Private | Public

type mutable_flag = Immutable | Mutable

type virtual_flag = Virtual | Concrete

type override_flag = Override | Fresh

type closed_flag = Closed | Open

type label = string

(* types *)

module Types = struct
	type record_representation =
		Record_regular                      (* All fields are boxed / tagged *)
	  | Record_float                        (* All fields are floats *)
end

(* primitive *)

module Primitive = struct
	type description =
	  { prim_name: string;         (* Name of primitive  or C function *)
		prim_arity: int;           (* Number of arguments *)
		prim_alloc: bool;          (* Does it allocates or raise? *)
		prim_native_name: string;  (* Name of C function for the nat. code gen. *)
		prim_native_float: bool }  (* Does the above operate on unboxed floats? *)
end

(* ident *)

module Ident = struct
	type t
end

(* lambda *)

type primitive =
    Pidentity
  | Pignore
    (* Globals *)
  | Pgetglobal of Ident.t
  | Psetglobal of Ident.t
  (* Operations on heap blocks *)
  | Pmakeblock of int * mutable_flag
  | Pfield of int
  | Psetfield of int * bool
  | Pfloatfield of int
  | Psetfloatfield of int
  | Pduprecord of Types.record_representation * int
  (* Force lazy values *)
  | Plazyforce
  (* External call *)
  | Pccall of Primitive.description
  (* Exceptions *)
  | Praise
  (* Boolean operations *)
  | Psequand | Psequor | Pnot
  (* Integer operations *)
  | Pnegint | Paddint | Psubint | Pmulint | Pdivint | Pmodint
  | Pandint | Porint | Pxorint
  | Plslint | Plsrint | Pasrint
  | Pintcomp of comparison
  | Poffsetint of int
  | Poffsetref of int
  (* Float operations *)
  | Pintoffloat | Pfloatofint
  | Pnegfloat | Pabsfloat
  | Paddfloat | Psubfloat | Pmulfloat | Pdivfloat
  | Pfloatcomp of comparison
  (* String operations *)
  | Pstringlength | Pstringrefu | Pstringsetu | Pstringrefs | Pstringsets
  (* Array operations *)
  | Pmakearray of array_kind
  | Parraylength of array_kind
  | Parrayrefu of array_kind
  | Parraysetu of array_kind
  | Parrayrefs of array_kind
  | Parraysets of array_kind
  (* Test if the argument is a block or an immediate integer *)
  | Pisint
  (* Test if the (integer) argument is outside an interval *)
  | Pisout
  (* Bitvect operations *)
  | Pbittest
  (* Operations on boxed integers (Nativeint.t, Int32.t, Int64.t) *)
  | Pbintofint of boxed_integer
  | Pintofbint of boxed_integer
  | Pcvtbint of boxed_integer (*source*) * boxed_integer (*destination*)
  | Pnegbint of boxed_integer
  | Paddbint of boxed_integer
  | Psubbint of boxed_integer
  | Pmulbint of boxed_integer
  | Pdivbint of boxed_integer
  | Pmodbint of boxed_integer
  | Pandbint of boxed_integer
  | Porbint of boxed_integer
  | Pxorbint of boxed_integer
  | Plslbint of boxed_integer
  | Plsrbint of boxed_integer
  | Pasrbint of boxed_integer
  | Pbintcomp of boxed_integer * comparison
  (* Operations on big arrays: (unsafe, #dimensions, kind, layout) *)
  | Pbigarrayref of bool * int * bigarray_kind * bigarray_layout
  | Pbigarrayset of bool * int * bigarray_kind * bigarray_layout

and comparison =
    Ceq | Cneq | Clt | Cgt | Cle | Cge

and array_kind =
    Pgenarray | Paddrarray | Pintarray | Pfloatarray

and boxed_integer =
    Pnativeint | Pint32 | Pint64

and bigarray_kind =
    Pbigarray_unknown
  | Pbigarray_float32 | Pbigarray_float64
  | Pbigarray_sint8 | Pbigarray_uint8
  | Pbigarray_sint16 | Pbigarray_uint16
  | Pbigarray_int32 | Pbigarray_int64
  | Pbigarray_caml_int | Pbigarray_native_int
  | Pbigarray_complex32 | Pbigarray_complex64

and bigarray_layout =
    Pbigarray_unknown_layout
  | Pbigarray_c_layout
  | Pbigarray_fortran_layout

type structured_constant =
    Const_base of constant
  | Const_pointer of int
  | Const_block of int * structured_constant list
  | Const_float_array of string list
  | Const_immstring of string

type meth_kind = Self | Public | Cached

(* debuginfo *)

module Debuginfo = struct
	type kind = Dinfo_call | Dinfo_raise

	type t = {
	  dinfo_kind: kind;
	  dinfo_file: string;
	  dinfo_line: int;
	  dinfo_char_start: int;
	  dinfo_char_end: int
	}
end

(* clambda *)

type function_label = string

type ulambda =
    Uvar of Ident.t
  | Uconst of structured_constant
  | Udirect_apply of function_label * ulambda list * Debuginfo.t
  | Ugeneric_apply of ulambda * ulambda list * Debuginfo.t
  | Uclosure of (function_label * int * Ident.t list * ulambda) list
              * ulambda list
  | Uoffset of ulambda * int
  | Ulet of Ident.t * ulambda * ulambda
  | Uletrec of (Ident.t * ulambda) list * ulambda
  | Uprim of primitive * ulambda list * Debuginfo.t
  | Uswitch of ulambda * ulambda_switch
  | Ustaticfail of int * ulambda list
  | Ucatch of int * Ident.t list * ulambda * ulambda
  | Utrywith of ulambda * Ident.t * ulambda
  | Uifthenelse of ulambda * ulambda * ulambda
  | Usequence of ulambda * ulambda
  | Uwhile of ulambda * ulambda
  | Ufor of Ident.t * ulambda * ulambda * direction_flag * ulambda
  | Uassign of Ident.t * ulambda
  | Usend of meth_kind * ulambda * ulambda * ulambda list * Debuginfo.t

and ulambda_switch =
  { us_index_consts: int array;
    us_actions_consts: ulambda array;
    us_index_blocks: int array;
    us_actions_blocks: ulambda array}

(* Description of known functions *)

type function_description =
  { fun_label: function_label;          (* Label of direct entry point *)
    fun_arity: int;                     (* Number of arguments *)
    mutable fun_closed: bool;           (* True if environment not used *)
    mutable fun_inline: (Ident.t list * ulambda) option }

(* Approximation of values *)

type value_approximation =
    Value_closure of function_description * value_approximation
  | Value_tuple of value_approximation array
  | Value_unknown
  | Value_integer of int
  | Value_constptr of int

(* compilenv *)

type unit_infos =
  { mutable ui_name: string;                    (* Name of unit implemented *)
    mutable ui_symbol: string;            (* Prefix for symbols *)
    mutable ui_defines: string list;      (* Unit and sub-units implemented *)
    mutable ui_imports_cmi: (string * Digest.t) list; (* Interfaces imported *)
    mutable ui_imports_cmx: (string * Digest.t) list; (* Infos imported *)
    mutable ui_approx: value_approximation;     (* Approx of the structure *)
    mutable ui_curry_fun: int list;             (* Currying functions needed *)
    mutable ui_apply_fun: int list;             (* Apply functions needed *)
    mutable ui_send_fun: int list;              (* Send functions needed *)
    mutable ui_force_link: bool }               (* Always linked *)

let cmx_magic_number = "Caml1999Y011"

(* from compilenv.ml *)
let digest_of filename_cmx =
	let ic = open_in_bin filename_cmx in
	try
		let buffer = String.create (String.length cmx_magic_number) in
		really_input ic buffer 0 (String.length cmx_magic_number);
		if buffer <> cmx_magic_number then begin
			close_in ic;
			failwith "not a cmx file";
		end;
		let ui = (input_value ic : unit_infos) in
		let crc = Digest.input ic in
		close_in ic;
		{
			name = ui.ui_name;
			imports_cmi = ui.ui_imports_cmi;
			imports_cmx = ui.ui_imports_cmx;
			defines = ui.ui_defines;
			crc
		}
	with End_of_file ->
		close_in ic;
		failwith "corrupted cmx"
