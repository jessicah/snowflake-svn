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

(* $Id: my_unix_with_unix.ml,v 1.1.4.1 2007/03/04 15:36:20 pouillar Exp $ *)
(* Original author: Nicolas Pouillard *)
open Format
open Ocamlbuild_pack
open My_unix

let report_error f =
  function
  | Unix.Unix_error(err, fun_name, arg) ->
      fprintf f "%s: %S failed" Sys.argv.(0) fun_name;
      if String.length arg > 0 then
        fprintf f " on %S" arg;
      fprintf f ": %s" (Unix.error_message err)
  | exn -> raise exn

let mkstat unix_stat x =
  let st =
    try unix_stat x
    with Unix.Unix_error _ as e -> raise (Sys_error (My_std.sbprintf "%a" report_error e))
  in
  { stat_key = sprintf "(%d,%d)" st.Unix.st_dev st.Unix.st_ino;
    stat_file_kind =
      match st.Unix.st_kind with
      | Unix.S_LNK -> FK_link
      | Unix.S_DIR -> FK_dir
      | Unix.S_CHR | Unix.S_BLK | Unix.S_FIFO | Unix.S_SOCK -> FK_other
      | Unix.S_REG -> FK_file }

let is_link s = (Unix.lstat s).Unix.st_kind = Unix.S_LNK

let at_exit_once callback =
  let pid = Unix.getpid () in
  at_exit begin fun () ->
    if pid = Unix.getpid () then callback ()
  end

let run_and_open s kont =
  let ic = Unix.open_process_in s in
  let close () =
    match Unix.close_process_in ic with
    | Unix.WEXITED 0 -> ()
    | Unix.WEXITED _ | Unix.WSIGNALED _ | Unix.WSTOPPED _ ->
        failwith (Printf.sprintf "Error while running: %s" s) in
  try
    let res = kont ic in close (); res
  with e -> (close (); raise e)

let stdout_isatty () =
  Unix.isatty Unix.stdout

let setup () =
  implem.is_degraded <- false;
  implem.stdout_isatty <- stdout_isatty;
  implem.gettimeofday <- Unix.gettimeofday;
  implem.report_error <- report_error;
  implem.execute_many <- Executor.execute;
  implem.readlink <- Unix.readlink;
  implem.run_and_open <- run_and_open;
  implem.at_exit_once <- at_exit_once;
  implem.is_link <- is_link;
  implem.stat <- mkstat Unix.stat;
  implem.lstat <- mkstat Unix.lstat;
