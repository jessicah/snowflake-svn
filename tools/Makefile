
OCAML = ocaml-3.10.2
OCAMLFILE = $(OCAML).touch
BITSTRING = ocaml-bitstring-2.0.0
BITSTRINGFILE = $(BITSTRING).touch
PWD = $(shell pwd)
OCAMLCMDS = "ocamlc=$(PWD)/ocaml/bin/ocamlc.opt ocamlopt=$(PWD)/ocaml/bin/ocamlopt.opt camlp4o=$(PWD)/ocaml/bin/camlp4o"

ocaml: $(OCAMLFILE)
	cd $(OCAML) && patch -Np0 -i ../ocamlbuild.snowflake.patch && ./configure -prefix $(PWD)/ocaml -no-curses -no-pthread -no-tk
	$(MAKE) -C $(OCAML) opt.opt install

$(OCAMLFILE):
	tar xjf $(OCAML).tar.bz2
	touch $(OCAMLFILE)

bitstring: $(BITSTRINGFILE) ocaml
	cd $(BITSTRING) && cp -f ../Makefile.in.bitstring Makefile.in && ./configure --prefix=$(PWD)/ocaml
	$(MAKE) -C $(BITSTRING) all install
	touch bitstring

$(BITSTRINGFILE):
	tar xzf $(BITSTRING).tar.gz
	touch $(BITSTRINGFILE)

clean:
	rm -rf ocaml $(OCAML) $(OCAMLFILE)
	rm -rf $(BITSTRING) $(BITSTRINGFILE) bitstring

cleanbitstring:
	rm -rf $(BITSTRING) $(BITSTRINGFILE) bitstring
