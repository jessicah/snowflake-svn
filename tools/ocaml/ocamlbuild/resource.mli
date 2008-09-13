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

(* $Id: resource.mli,v 1.1 2007/02/07 08:59:14 ertai Exp $ *)
(* Original author: Nicolas Pouillard *)
open My_std

open Pathname
type env

module Resources : Set.S with type elt = t

module Cache :
  sig
    type suspension

    type build_status =
      | Bbuilt
      | Bcannot_be_built
      | Bnot_built_yet
      | Bsuspension of suspension

    val clean : unit -> unit
    val init : unit -> unit
    val resource_state : t -> build_status
    val resource_changed : t -> unit
    val resource_has_changed : t -> bool
    val resource_is_built : t -> bool
    val resource_built : t -> unit
    val resource_is_failed : t -> bool
    val resource_failed : t -> unit
    val suspend_resource : t -> Command.t -> (unit -> unit) -> t list -> unit
    val resume_resource : t -> unit
    val resume_suspension : suspension -> unit
    val get_optional_resource_suspension : t -> (Command.t * (unit -> unit)) option
    val clear_resource_failed : t -> unit
    val dependencies : t -> Resources.t
    val add_dependency : t -> t -> unit
    val get_digest_for : string -> string option
    val store_digest : string -> string -> unit
    val digest_resource : t -> string
    val print_cache : Format.formatter -> unit -> unit
    val print_dependencies : Format.formatter -> unit -> unit
    val fold_dependencies : (string -> string -> 'a -> 'a) -> 'a -> 'a
  end

val compare : t -> t -> int
val print : Format.formatter -> t -> unit
val clean : t -> unit
val import : string -> t

val matchit : t -> t -> env option
val subst : env -> t -> t
val is_up_to_date : t -> bool
val print_env : Format.formatter -> env -> unit
