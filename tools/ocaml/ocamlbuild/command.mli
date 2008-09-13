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

(* $Id: command.mli,v 1.1.4.1 2007/03/23 16:34:48 pouillar Exp $ *)
(* Original author: Nicolas Pouillard *)
(* Command *)

(** Provides an abstract type for easily building complex shell commands without making
    quotation mistakes.  *)
include Signatures.COMMAND with type tags = Tags.t

(** {6 For system use only, not for the casual user} *)

val string_target_and_tags_of_command_spec : spec -> string * string * Tags.t

(** Same as [to_string]. *)
val to_string_for_digest : t -> string

(** Maximum number of parallel jobs. *)
val jobs : int ref

(** Hook here the function that maps a set of tags to appropriate command
    options. It also build the dependencies that matches the tags. *)
val tag_handler : (Tags.t -> spec) ref
