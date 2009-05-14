(***********************************************************************)
(*                                                                     *)
(*                         Objective Caml                              *)
(*                                                                     *)
(*  Xavier Leroy and Pascal Cuoq, projet Cristal, INRIA Rocquencourt   *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../../LICENSE.  *)
(*                                                                     *)
(***********************************************************************)

(* $Id: thread_posix.ml,v 1.10 2005-07-31 12:32:41 xleroy Exp $ *)

(* User-level threads *)

type t

external thread_initialize : unit -> unit = "caml_thread_initialize"
external thread_new : (unit -> unit) -> string -> t = "caml_thread_new"
external thread_uncaught_exception : exn -> unit = 
            "caml_thread_uncaught_exception"

external yield : unit -> unit = "caml_thread_yield"
external self : unit -> t = "caml_thread_self"
external id : t -> int = "caml_thread_id"
external join : t -> unit = "caml_thread_join"
external exit : unit -> unit = "caml_thread_exit"
external sleep : unit -> unit = "caml_thread_sleep"
external wake : t -> unit = "caml_thread_wake"

external usleep : int -> unit = "snowflake_thread_usleep"

(* For new, make sure the function passed to thread_new never
   raises an exception. *)

let create fn arg name =
  thread_new
    (fun () ->
      try
        fn arg; ()
      with exn ->
             thread_uncaught_exception exn) name

(* Thread.kill is currently not implemented due to problems with
   cleanup handlers on several platforms *)

let kill th = invalid_arg "Thread.kill: not implemented"

(* Preemption *)

let preempt signal = yield()

(* Initialization of the scheduler *)

let _ =
  thread_initialize()
