
let () =
    Vga.init (); (* set up a pretty console font *)
    Vt100.printf "Hello, from ML :)\nUsing ocaml version: %s\n" Sys.ocaml_version
