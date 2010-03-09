
all: elf_linker tiny tiny2 tiny3

elf_linker: linkerTest.mli linkerTest.ml ELF.mli ELF.ml
	ocamlfind ocamlc -package bitstring,bitstring.syntax,extlib -syntax bitstring.syntax -linkpkg $+ -o elf_linker

%.o: %.S
	as --32 $< -o $@

tiny: tiny.o
	ld -Telf_i386.x -s -melf_i386 $< -o $@

tiny2: tiny1.o tiny2.o
	ld -Telf_i386.x -s -melf_i386 $+ -o $@

tiny3: tinya.o tinyb.o
	ld -Telf_i386.x -s -melf_i386 $+ -o $@

clean:
	rm -f *.cm[io] *.o ELF elf_linker tiny tiny2 tiny3
