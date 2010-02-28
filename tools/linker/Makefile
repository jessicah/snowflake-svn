
all: linkerTest.mli linkerTest.ml ELF.mli ELF.ml
	ocamlfind ocamlc -package bitstring,bitstring.syntax -syntax bitstring.syntax -linkpkg $+ -o ELF

clean:
	rm *.cm[io] ELF
