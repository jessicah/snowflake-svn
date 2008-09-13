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

(* $Id: configuration.mli,v 1.1 2007/02/07 08:59:13 ertai Exp $ *)
(* Original author: Nicolas Pouillard *)
(* Configuration *)

(** Handles the "_tags" file mechanism. *)

type flag_list = (string * string) list

(** Incorporate a newline-separated configuration string into the current configuration.
    Will usually raising an [Invalid_arg] with an appropriately explicit message in case of error. *)
val parse_string : string -> unit

(** [parse_file ?dir fn] incorporates the configuration file named [fn], prefixing its glob patterns
    with [dir] if given. *)
val parse_file : ?dir:string -> string -> unit

(** Return the set of tags that apply to a given filename under the current configuration. *)
val tags_of_filename : string -> Tags.t

(** Return the set of flags that apply to a given filename under the current configuration. *)
val flags_of_filename : string -> Command.spec

val has_tag : string -> bool
