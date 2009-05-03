
open Printf
open Sys

let args = match Array.to_list argv with
    | [] -> assert false
    | _ :: xs -> xs

let prefix = "tools/ocamlopt/"
let prefix_len = String.length prefix

let config = "myocamlbuild_config"
let config_len = String.length config

let use_system_compiler =
    List.exists begin fun arg ->
        if String.length arg > prefix_len then begin
            let s = String.sub arg 0 prefix_len in
            if String.compare s prefix = 0 then
                true
            else begin
                if String.length arg > config_len then begin
                    let s = String.sub arg 0 config_len in
                    String.compare s config = 0
                end else
                    false
            end
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
        let cmd = sprintf "../tools/ocaml/bin/ocamlopt.opt %s" args in
        exit (command cmd)
    end else begin
        let cmd = sprintf "./ocamlopt.opt %s" args in
        exit (command cmd)
    end
