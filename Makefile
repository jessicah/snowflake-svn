
BUILDDIR = _build

OCB = tools/ocaml/bin/ocamlbuild 

OCAMLBUILD = $(OCB) $(OCBFLAGS)

KERNEL = kernel/snowflake.native

ISO = snowflake.iso

all: myocamlbuild_config.ml
	$(MAKE) -C tools ocaml bitstring
	$(OCAMLBUILD) libraries/stdlib/stdlib.cmxa libraries/extlib/extlib.cmxa libraries/threads/threads.cmxa libraries/bitstring/bitstring.cmxa $(KERNEL)
	rm -rf cdrom/iso_prep
	# this should be done in myocamlbuild
	#strip -s $(BUILDDIR)/$(KERNEL)
	mkdir -p cdrom/iso_prep/boot/grub/
	cp cdrom/stage2_eltorito cdrom/iso_prep/boot/grub/
	cp $(BUILDDIR)/$(KERNEL) cdrom/iso_prep/boot/snowflake.elf
	cp cdrom/menu.lst cdrom/iso_prep/boot/grub/
	cp example.wav cdrom/iso_prep/
	mkisofs -R -b boot/grub/stage2_eltorito -no-emul-boot \
		-boot-load-size 4 -boot-info-table \
		-quiet -o $(ISO) cdrom/iso_prep/
	rm -f file.lst files.tar

myocamlbuild_config.ml: myocamlbuild_config.ml.in
	sed -e 's/@TOOLSPREFIX@/$(subst /,\/,$(TOOLSPREFIX))/' $< > $@

qemu:
	qemu -serial stdio -boot d -hda fake_hd.tar -cdrom snowflake.iso -soundhw all -net nic,model=rtl8139,macaddr=00:15:60:9E:28:0A -net tap -m 512 -no-kqemu

clean:
	$(OCAMLBUILD) -clean || true
	rm -f $(ISO)
	rm -rf cdrom/iso_prep
	rm -f myocamlbuild_config.ml

distclean: clean
	$(MAKE) -C tools clean
