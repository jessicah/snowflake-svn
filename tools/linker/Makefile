
all: linkerTest.mli linkerTest.ml ELF.mli ELF.ml
	ocamlfind ocamlc -package bitstring,bitstring.syntax,extlib -syntax bitstring.syntax -linkpkg $+ -o elf_linker

%.o: %.S
	as --32 $< -o $@

tiny: tiny.o
	ld -s -melf_i386 $< -o $@

tiny2: tinya.o tinyb.o
	ld -s -melf_i386 $+ -o $@

clean:
	rm -f *.cm[io] *.o ELF elf_linker tiny
