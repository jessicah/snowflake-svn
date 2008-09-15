
OCAML = ocaml-3.10.2
OCAMLFILE = $(OCAML).touch
PWD = $(shell pwd)

ocaml: $(OCAMLFILE)
	cd $(OCAML) && patch -Np0 -i ../ocamlbuild.snowflake.patch && patch -Np0 -i ../nocamlp4.patch && ./configure -prefix $(PWD)/ocaml -no-curses -no-pthread -no-tk
	$(MAKE) -C $(OCAML) opt.opt install

$(OCAMLFILE):
	tar xjf $(OCAML).tar.bz2
	touch $(OCAMLFILE)

clean:
	rm -rf ocaml $(OCAML) $(OCAMLFILE)
