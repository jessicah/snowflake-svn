(***********************************************************************)
(*                                                                     *)
(*                         Objective Caml                              *)
(*                                                                     *)
(*  Xavier Leroy and Pascal Cuoq, projet Cristal, INRIA Rocquencourt   *)
(*                                                                     *)
(*  Copyright 1995 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../../LICENSE.  *)
(*                                                                     *)
(***********************************************************************)

(* $Id: thread.mli,v 1.20 2005-07-31 12:32:41 xleroy Exp $ *)

(** Lightweight threads for Posix [1003.1c] and Win32. *)

type t
(** The type of thread handles. *)

(** {6 Thread creation and termination} *)

val create : ('a -> 'b) -> 'a -> string -> t
(** [Thread.create funct arg] creates a new thread of control,
   in which the function application [funct arg]
   is executed concurrently with the other threads of the program.
   The application of [Thread.create]
   returns the handle of the newly created thread.
   The new thread terminates when the application [funct arg]
   returns, either normally or by raising an uncaught exception.
   In the latter case, the exception is printed on standard error,
   but not propagated back to the parent thread. Similarly, the
   result of the application [funct arg] is discarded and not
   directly accessible to the parent thread. *)

external self : unit -> t = "caml_thread_self"
(** Return the thread currently executing. *)

external id : t -> int = "caml_thread_id"
(** Return the identifier of the given thread. A thread identifier
   is an integer that identifies uniquely the thread.
   It can be used to build data structures indexed by threads. *)

val exit : unit -> unit
(** Terminate prematurely the currently executing thread. *)

val kill : t -> unit
(** Terminate prematurely the thread whose handle is given. *)

(** {6 Suspending threads} *)

external join : t -> unit = "caml_thread_join"
(** [join th] suspends the execution of the calling thread
   until the thread [th] has terminated. *)

val yield : unit -> unit
(** Re-schedule the calling thread without suspending it.
   This function can be used to give scheduling hints,
   telling the scheduler that now is a good time to
   switch to other threads. *)

val sleep : unit -> unit
(** Put the calling thread to sleep. *)

val wake : t -> unit
(** [wake th] wakes up the thread [th]. *)
