
BUILDDIR = _build

OCB = tools/custom/bin/ocamlbuild 

OCAMLBUILD = $(OCB) $(OCBFLAGS)

KERNEL = kernel/snowflake.native

ISO = snowflake.iso

all: myocamlbuild_config.ml
	if [ ! -e tools/Makefile ] ; then cp -f tools/Makefile.in tools/Makefile; fi
	$(MAKE) -C tools all
	$(OCAMLBUILD) ocamlopt.opt
	$(OCAMLBUILD) libraries/dummy/dlldummy.so
	$(OCAMLBUILD) libraries/stdlib/stdlib.cmxa libraries/bigarray/bigarray.cmxa libraries/extlib/extlib.cmxa libraries/threads/threads.cmxa libraries/bitstring/bitstring.cmxa $(KERNEL)
	rm -rf cdrom/iso_prep
	# this should be done in myocamlbuild
	#strip -s $(BUILDDIR)/$(KERNEL)
	cd $(BUILDDIR) && find -name '*.o' -or -name '*.a' | sed -e 's/\.\///' > ../file.lst
	echo kernel/snowflake.native >> file.lst
	cd $(BUILDDIR) && tar cf ../files.tar -T ../file.lst
	mkdir -p cdrom/iso_prep/boot/grub/
	cp cdrom/stage2_eltorito cdrom/iso_prep/boot/grub/
	cp $(BUILDDIR)/$(KERNEL) cdrom/iso_prep/boot/snowflake.elf
	cp cdrom/menu.lst cdrom/iso_prep/boot/grub/
	#touch example.wav # think it need be around for grub menu.lst file...
	cp files.tar cdrom/iso_prep/
	#cp example.wav cdrom/iso_prep/
	mkisofs -R -b boot/grub/stage2_eltorito -no-emul-boot \
		-boot-load-size 4 -boot-info-table \
		-quiet -o $(ISO) cdrom/iso_prep/
	rm -f file.lst files.tar

archive.tar:
	$(OCAMLBUILD) -tag plugin -classic-display plugins/irc.cmxs
	cp $(BUILDDIR)/plugins/irc.cmxs .
	$(MAKE) -C plugins/ocamlopt.opt all
	cp plugins/ocamlopt.opt/optmain.cmxs .
	tar cf $@ irc.cmxs optmain.cmxs

myocamlbuild_config.ml: myocamlbuild_config.ml.in
	sed -e 's/@TOOLSPREFIX@/$(subst /,\/,$(TOOLSPREFIX))/' $< > $@

prep-osx:
	cp -f tools/Makefile.in tools/Makefile
	cd tools && patch -i ../macosx.patch

qemu:
	qemu -serial stdio -boot d -hda fake_hd.tar -cdrom snowflake.iso -soundhw all -net nic,model=rtl8139,macaddr=00:15:60:9E:28:0A -net user -m 512

clean:
	$(OCAMLBUILD) -clean || true
	$(MAKE) -C plugins/ocamlopt.opt clean
	rm -f $(ISO)
	rm -rf cdrom/iso_prep
	rm -f myocamlbuild_config.ml
	rm -f archive.tar

distclean: clean
	$(MAKE) -C tools clean
	rm -f tools/Makefile
