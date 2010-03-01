
all: linkerTest.mli linkerTest.ml ELF.mli ELF.ml
	ocamlfind ocamlc -package bitstring,bitstring.syntax -syntax bitstring.syntax -linkpkg $+ -o elf_linker

clean:
	rm -f *.cm[io] ELF elf_linker
