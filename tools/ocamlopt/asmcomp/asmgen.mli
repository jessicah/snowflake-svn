(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id: asmgen.mli,v 1.7.34.1 2007-06-14 12:07:11 frisch Exp $ *)

(* From lambda to assembly code *)

val compile_implementation :
    ?toplevel:(string -> bool) ->
    string -> Format.formatter -> int * Lambda.lambda -> unit
val compile_phrase :
    Format.formatter -> Cmm.phrase -> unit

type error = Assembler_error of string
exception Error of error
val report_error: Format.formatter -> error -> unit
