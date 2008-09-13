(***********************************************************************)
(*                             ocamlbuild                              *)
(*                                                                     *)
(*  Nicolas Pouillard, Berke Durak, projet Gallium, INRIA Rocquencourt *)
(*                                                                     *)
(*  Copyright 2007 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id: lexers.mli,v 1.2 2007/02/16 10:35:10 ertai Exp $ *)
(* Original author: Nicolas Pouillard *)
exception Error of string

type conf_values =
  { plus_tags   : string list;
    minus_tags  : string list;
    plus_flags  : (string * string) list;
    minus_flags : (string * string) list }

type conf = (Glob.globber * conf_values) list

val ocamldep_output : Lexing.lexbuf -> (string * string list) list
val space_sep_strings : Lexing.lexbuf -> string list
val blank_sep_strings : Lexing.lexbuf -> string list
val comma_sep_strings : Lexing.lexbuf -> string list
val comma_or_blank_sep_strings : Lexing.lexbuf -> string list
val colon_sep_strings : Lexing.lexbuf -> string list
val conf_lines : string option -> int -> string -> Lexing.lexbuf -> conf
val meta_path : Lexing.lexbuf -> (string * bool) list
