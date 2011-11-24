(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*       Nicolas Pouillard, projet Gallium, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 2007 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

open Ocamlbuild_plugin
open Command
open Arch
open Format

module C = Myocamlbuild_config

let ccomptype = C.ccomptype
let () = if ccomptype <> "cc" then eprintf "ccomptype: %s@." ccomptype;;

let fp_cat oc f = with_input_file ~bin:true f (fun ic -> copy_chan ic oc)

(*(* Improve using the command module in Myocamlbuild_config
   with the variant version (`S, `A...) *)
let mkdll out files opts =
  let s = Command.string_of_command_spec in
  Cmd(Sh(Printf.sprintf "%s -o %s %s %s" C.mkdll out (s files) (s opts)))

let mkexe out files opts =
  let s = Command.string_of_command_spec in
  Cmd(Sh(Printf.sprintf "%s -o %s %s %s" C.mkexe out (s files) (s opts)))

let mklib out files opts =
  let s = Command.string_of_command_spec in
  Cmd(Sh(C.mklib out (s files) (s opts)))

let syslib x = A(C.syslib x);;
let syscamllib x =
  if ccomptype = "msvc" then A(Printf.sprintf "lib%s.lib" x)
  else A("-l"^x)

let mkobj obj file opts =
  let obj = obj-.-C.o in
  if ccomptype = "msvc" then
    Seq[Cmd(S[Sh C.bytecc; Sh C.bytecccompopts; opts; A"-c"; Px file]);
        mv (Pathname.basename (Pathname.update_extension C.o file)) obj]
  else
    Cmd(S[Sh C.bytecc; Sh C.bytecccompopts; opts; A"-c"; P file; A"-o"; Px obj])

let mkdynobj obj file opts =
  let d_obj = obj-.-"d"-.-C.o in
  if ccomptype = "msvc" then
    Seq[Cmd(S[Sh C.bytecc; opts; Sh C.dllcccompopts; A"-c"; Px file]);
        mv (Pathname.basename (Pathname.update_extension C.o file)) d_obj]
  else
    Cmd(S[Sh C.bytecc; opts; Sh C.dllcccompopts; A"-c"; P file; A"-o"; Px d_obj])

let mknatobj obj file opts =
  let obj = obj-.-C.o in
  if ccomptype = "msvc" then
    Seq[Cmd(S[Sh C.nativecc; opts; A"-c"; Px file]);
        mv (Pathname.basename (Pathname.update_extension C.o file)) obj]
  else
    Cmd(S[Sh C.nativecc; A"-O"; opts;
          Sh C.nativecccompopts; A"-c"; P file; A"-o"; Px obj])
*)

let ar = A"ar";;

dispatch begin function
| After_options ->
    begin
      Options.ext_obj := C.o;
      Options.ext_lib := C.a;
      Options.ext_dll := String.after C.ext_dll 1;

      Options.nostdlib := true;
      Options.make_links := false;
      Options.ocamlc := Sh"../../../_build/ocamlopt.opt";
      Options.ocamlopt := Sh"../../../_build/ocamlopt.opt";
    end
| After_rules ->
    let module M = struct
    
flag ["ocaml";"compile"] (A"-nostdlib");;
flag ["ocaml";"link"] (A"-nostdlib");;
    
    flag ["ocaml";"compile"] & S[A"-I";A"../../../_build/libraries/stdlib"];;
	flag ["ocaml";"compile"] & S[A"-I";A"../../../_build/kernel"];;

flag ["ocaml"; "ocamlyacc"] (A"-v");;

flag ["ocaml"; "native"; "shared"; "library"]
	& S[A"-cclib"; A"-nostdlib"; A"-cclib"; A"-Wl,-nostdlib"; A"-cclib"; Sh"-Wl,-hash-style=sysv"];;


let add_extensions extensions modules =
  List.fold_right begin fun x ->
    List.fold_right begin fun ext acc ->
      x-.-ext :: acc
    end extensions
  end modules [];;

(*flag ["ocaml"; "link"; "file:driver/main.native"; "native"] begin
  S[A"-ccopt"; A C.bytecclinkopts; A"-cclib"; A C.bytecclibs]
end;;*)

(*dep ["ocaml"; "link"; "file:driver/main.native"; "native"]
    ["../../../_build/libraries/asmrun/meta"-.-C.o; "../../../_build/libraries/asmrun/dynlink"-.-C.o];;

dep ["ocaml"; "compile"; "native"] ["../../../_build/libraries/asmrun/libasmrun"-.-C.a];;*)

(*dep ["ocaml";"link";"native"] ["std_exit.cmx"];;*)
flag ["ocaml";"link";"native"] (A"-freestanding");;

flag ["ocaml"; "link"] (S[A"-I"; P "../../../_build/libraries/stdlib"]);;

let setup_arch arch =
  let annotated_arch = annotate arch in
  let (_include_dirs_table, _for_pack_table) = mk_tables annotated_arch in
  (* Format.eprintf "%a@." (Ocaml_arch.print_table (List.print pp_print_string)) include_dirs_table;; *)
  iter_info begin fun i ->
    Pathname.define_context i.current_path i.include_dirs
  end annotated_arch;;

Pathname.define_context "parsing" ["parsing"; "utils";];;
Pathname.define_context "typing" ["typing"; "parsing"; "utils";];;
Pathname.define_context "bytecomp" ["bytecomp"; "parsing"; "typing"; "utils";];;
Pathname.define_context "driver" ["driver"; "asmcomp"; "bytecomp"; "typing"; "utils"; "parsing";];;
Pathname.define_context "asmcomp" ["asmcomp"; "bytecomp"; "parsing"; "typing"; "utils";];;

let copy_rule' ?insert src dst = copy_rule (sprintf "%s -> %s" src dst) ?insert src dst;;

(*copy_rule' "driver/main.byte" "ocamlc";;
copy_rule' "driver/main.native" "ocamlc.opt";;
copy_rule' "driver/optmain.byte" "ocamlopt";;
copy_rule' "driver/optmain.native" "ocamlopt.opt";;*)

rule "bytecomp/runtimedef.ml"
  ~prod:"bytecomp/runtimedef.ml"
  ~deps:["byterun/primitives"; "byterun/fail.h"]
  begin fun _ _ ->
    Cmd(S[A"../build/mkruntimedef.sh";Sh">"; Px"bytecomp/runtimedef.ml"])
  end;;

(* Choose the right machine-dependent files *)

let mk_arch_rule ~src ~dst =
  let prod = "asmcomp"/dst in
  let dep = "asmcomp"/C.arch/src in
  rule (sprintf "arch specific files %S%%" dst) ~prod ~dep begin
    fun env _ -> ln_s (env (C.arch/src)) (env prod)
  end;;

mk_arch_rule ~src:(if ccomptype = "msvc" then "proc_nt.ml" else "proc.ml") ~dst:"proc.ml";;
List.iter (fun x -> mk_arch_rule ~src:x ~dst:x)
          ["arch.ml"; "reload.ml"; "scheduling.ml"; "selection.ml"];;

let emit_mlp = "asmcomp"/C.arch/(if ccomptype = "msvc" then "emit_nt.mlp" else "emit.mlp") in
rule "emit.mlp"
  ~prod:"asmcomp/emit.ml"
  ~deps:[emit_mlp; "tools/cvt_emit.byte"]
  begin fun _ _ ->
    Cmd(S[P"../../tools/custom/bin/ocamlrun"; P"tools/cvt_emit.byte"; Sh "<"; P emit_mlp;
          Sh">"; Px"asmcomp/emit.ml"])
  end;;

	end in ()
  | _ -> ()
end 