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

(* $Id: log.mli,v 1.1 2007/02/07 08:59:14 ertai Exp $ *)
(* Original author: Nicolas Pouillard *)
(* Log *)

(** Module for modulating the logging output with the logging level. *)
include Signatures.LOG

(** Turn it to true to have a classic display of commands. *)
val classic_display : bool ref

(** The optional log file. *)
val log_file : string option Lazy.t ref

(** See {Display.event}. *)
val event : ?pretend:bool -> string -> string -> Tags.t -> unit

(**/**)

val internal_display : Display.display Lazy.t
val finish : ?how:[`Success|`Error|`Quiet] -> unit -> unit
val display : (out_channel -> unit) -> unit
val update : unit -> unit
val mode : string -> bool
