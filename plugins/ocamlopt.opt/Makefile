all:
	../../tools/custom/bin/ocamlbuild -lflag -linkall -I bytecomp -I asmcomp -I utils -I driver -I parsing -I typing  -classic-display plugin.cmxs
	cp _build/plugin.cmxs optmain.cmxs

clean:
	../../tools/custom/bin/ocamlbuild -clean

