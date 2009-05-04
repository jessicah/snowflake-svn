
OCAML = ocaml-3.10.2
OCAMLFILE = $(OCAML).touch
BITSTRING = ocaml-bitstring-2.0.0
BITSTRINGFILE = $(BITSTRING).touch
PWD = $(shell pwd)

ocaml: $(OCAMLFILE)
	cd $(OCAML) && patch -Np0 -i ../ocamlbuild.snowflake.patch && ./configure -prefix $(PWD)/ocaml -no-curses -no-pthread -no-tk
	$(MAKE) -C $(OCAML) opt.opt install

$(OCAMLFILE):
	tar xjf $(OCAML).tar.bz2
	touch $(OCAMLFILE)

bitstring: $(BITSTRINGFILE) ocaml
	cd $(BITSTRING) && patch Makefile.in ../ocaml-bitstring.makefile.patch && ./configure --prefix=$(PWD)/ocaml
	OCAMLFIND_DESTDIR=$(PWD)/ocaml/lib/ocaml $(MAKE) -C $(BITSTRING) all install
	touch bitstring

$(BITSTRINGFILE):
	tar xzf $(BITSTRING).tar.gz
	touch $(BITSTRINGFILE)

clean:
	rm -rf ocaml $(OCAML) $(OCAMLFILE)
	rm -rf $(BITSTRING) $(BITSTRINGFILE) bitstring

cleanbitstring:
	rm -rf $(BITSTRING) $(BITSTRINGFILE) bitstring
