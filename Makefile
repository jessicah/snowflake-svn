
BUILDDIR = .build

OCB = ocamlbuild

OCAMLBUILD = $(OCB) $(OCBFLAGS) -log .log -build-dir $(BUILDDIR) -X $(BUILDDIR)

KERNEL = snowflake.native

ISO = snowflake.iso

all:
	$(OCAMLBUILD) kernel/$(KERNEL)
	rm -rf cdrom/iso_prep
	mkdir -p cdrom/iso_prep/boot/grub/
	cp cdrom/stage2_eltorito cdrom/iso_prep/boot/grub/
	cp $(KERNEL) cdrom/iso_prep/boot/snowflake.elf
	cp cdrom/menu.lst cdrom/iso_prep/boot/grub/
	mkisofs -R -b boot/grub/stage2_eltorito -no-emul-boot \
		-boot-load-size 4 -boot-info-table \
		-quiet -o $(ISO) cdrom/iso_prep/

clean:
	$(OCAMLBUILD) -clean
	rm -f $(ISO)
	rm -rf cdrom/iso_prep
