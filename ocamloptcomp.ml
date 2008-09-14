
open Printf
open Sys

let args = match Array.to_list argv with
    | [] -> assert false
    | _ :: xs -> xs

let prefix = "tools/ocamlopt/"
let prefix_len = String.length prefix

let use_system_compiler =
    List.exists begin fun arg ->
        if String.length arg > prefix_len then begin
            let s = String.sub arg 0 prefix_len in
            String.compare s prefix = 0
        end else
            false
    end args

let () =
    let args = List.map begin fun arg ->
        if String.contains arg ' ' then
            sprintf "'%s'" arg
        else
            arg
    end args in        
    let args = String.concat " " args in
    if use_system_compiler then begin
        let cmd = sprintf "/usr/bin/ocamlopt.opt %s" args in
        (*printf "+ %s\n" cmd;*)
        exit (command cmd)
    end else begin
        let cmd = sprintf "./ocamlopt.opt %s" args in
        (*printf "+ %s\n" cmd;*)
        exit (command cmd)
    end
