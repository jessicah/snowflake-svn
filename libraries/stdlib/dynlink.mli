(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../../LICENSE.  *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(** Dynamic loading of object files. *)

val is_native: bool
(** [true] if the program is native,
    [false] if the program is bytecode. *)

(** {6 Dynamic loading of compiled files} *)

val loadfile : string -> unit
(** In bytecode: load the given bytecode object file ([.cmo] file) or
    bytecode library file ([.cma] file), and link it with the running
    program. In native code: load the given OCaml plugin file (usually
    [.cmxs]), and link it with the running
    program.
    All toplevel expressions in the loaded compilation units
    are evaluated. No facilities are provided to
    access value names defined by the unit. Therefore, the unit
    must register itself its entry points with the main program,
    e.g. by modifying tables of functions. *)

val loadfile_private : string -> unit
(** Same as [loadfile], except that the compilation units just loaded
    are hidden (cannot be referenced) from other modules dynamically
    loaded afterwards. *)

(*val adapt_filename : string -> string
(** In bytecode, the identity function. In native code, replace the last
    extension with [.cmxs]. *)*)

(** {6 Access control} *)

val allow_only: string list -> unit
(** [allow_only units] restricts the compilation units that dynamically-linked
    units can reference: it only allows references to the units named in
    list [units].  References to any other compilation unit will cause
    a [Unavailable_unit] error during [loadfile] or [loadfile_private].

    Initially (just after calling [init]), all compilation units composing
    the program currently running are available for reference from
    dynamically-linked units.  [allow_only] can be used to grant access
    to some of them only, e.g. to the units that compose the API for
    dynamically-linked code, and prevent access to all other units,
    e.g. private, internal modules of the running program. *)

val prohibit: string list -> unit
(** [prohibit units] prohibits dynamically-linked units from referencing
    the units named in list [units].  This can be used to prevent
    access to selected units, e.g. private, internal modules of
    the running program. *)

val default_available_units: unit -> unit
(** Reset the set of units that can be referenced from dynamically-linked
    code to its default value, that is, all units composing the currently
    running program. *)

val allow_unsafe_modules : bool -> unit
(** Govern whether unsafe object files are allowed to be
    dynamically linked. A compilation unit is ``unsafe'' if it contains
    declarations of external functions, which can break type safety.
    By default, dynamic linking of unsafe object files is
    not allowed. In native code, this function does nothing; object files
    with external functions are always allowed to be dynamically linked. *)

(** {6 Error reporting} *)

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
(** Errors in dynamic linking are reported by raising the [Error]
    exception with a description of the error. *)

val error_message : error -> string
(** Convert an error description to a printable message. *)

