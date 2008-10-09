
BUILDDIR = _build

OCB = tools/ocaml/bin/ocamlbuild 

OCAMLBUILD = $(OCB) $(OCBFLAGS)

KERNEL = kernel/snowflake.native

ISO = snowflake.iso

all:
	gcc -c -m32 test1.c
	gcc -c -m32 test2.c
	ld test1.o test2.o -melf_i386 --unresolved-symbols=ignore-all -o test.out
	$(MAKE) -C tools ocaml bitstring
	$(OCAMLBUILD) libraries/stdlib/stdlib.cmxa libraries/extlib/extlib.cmxa libraries/threads/threads.cmxa libraries/bitstring/bitstring.cmxa $(KERNEL)
	rm -rf cdrom/iso_prep
	mkdir -p cdrom/iso_prep/boot/grub/
	cp cdrom/stage2_eltorito cdrom/iso_prep/boot/grub/
	cp $(BUILDDIR)/$(KERNEL) cdrom/iso_prep/boot/snowflake.elf
	cp cdrom/menu.lst cdrom/iso_prep/boot/grub/
	mkisofs -R -b boot/grub/stage2_eltorito -no-emul-boot \
		-boot-load-size 4 -boot-info-table \
		-quiet -o $(ISO) cdrom/iso_prep/

clean:
	$(OCAMLBUILD) -clean || true
	rm -f $(ISO)
	rm -rf cdrom/iso_prep
	rm -f test1.o test2.o test.out

distclean:
	$(MAKE) -C tools clean
