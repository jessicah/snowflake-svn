# Bitstring syntax extension.
# Copyright (C) 2008 Red Hat Inc., Richard W.M. Jones
#
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public
# License as published by the Free Software Foundation; either
# version 2 of the License, or (at your option) any later version.
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public
# License along with this library; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA
#
# $Id: Makefile.in 160 2008-08-27 11:27:47Z richard.wm.jones $

PACKAGE		= @PACKAGE_NAME@
VERSION		= @PACKAGE_VERSION@

OCAMLFIND	= @OCAMLFIND@
OCAMLMKLIB	= @OCAMLMKLIB@
OCAMLDOC	= @OCAMLDOC@
INSTALL		= @INSTALL@
TIME		= @TIME@
GPROF		= @GPROF@
BISECT_REPORT	= @BISECT_REPORT@

BYTESWAP_H	= @BYTESWAP_H@

OCAMLLIB	= @OCAMLLIB@
top_srcdir	= @top_srcdir@

pkg_cil		= @pkg_cil@
pkg_extlib	= @pkg_extlib@

OCAMLCFLAGS	= -g
OCAMLCPACKAGES	=
OCAMLCLIBS	= -linkpkg
OCAMLOPTFLAGS	=
OCAMLOPTPACKAGES =
OCAMLOPTLIBS	= -linkpkg

CC		= @CC@
CFLAGS		= @CFLAGS@ -Wall -Werror -fPIC -I$(top_srcdir) -I$(OCAMLLIB)

LIBRARY_PATH	= @abs_top_builddir@
LD_LIBRARY_PATH	= @abs_top_builddir@
export LIBRARY_PATH
export LD_LIBRARY_PATH

all:	$(BYTESWAP_H) \
	bitstring.cma bitstring_persistent.cma \
	bitstring.cmxa bitstring_persistent.cmxa \
	pa_bitstring.cmo \
	bitstring-objinfo
	@for d in $(SUBDIRS); do $(MAKE) -C $$d $@; done

bitstring.cma: bitstring_types.cmo bitstring_config.cmo \
	  bitstring_c.o bitstring.cmo bitmatch.cmo
	$(OCAMLMKLIB) -o bitstring $^

bitstring_persistent.cma: bitstring_persistent.cmo
	$(OCAMLFIND) ocamlc -a -o $@ $^

bitstring_persistent.cmo: bitstring_persistent.ml
	$(OCAMLFIND) ocamlc $(OCAMLCFLAGS) $(OCAMLCPACKAGES) \
	  -I +camlp4 -pp camlp4of -c $<

bitstring.cmxa: bitstring_types.cmx bitstring_config.cmx \
	  bitstring_c.o bitstring.cmx bitmatch.cmx
	$(OCAMLMKLIB) -o bitstring $^

bitstring_persistent.cmxa: bitstring_persistent.cmx
	$(OCAMLFIND) ocamlopt -a -o $@ $^

bitstring_persistent.cmx: bitstring_persistent.ml
	$(OCAMLFIND) ocamlopt $(OCAMLOPTFLAGS) $(OCAMLOPTPACKAGES) \
	  -I +camlp4 -pp camlp4of -c $<

bitstring_persistent.cmi: bitstring_persistent.mli
	$(OCAMLFIND) ocamlc $(OCAMLCFLAGS) $(OCAMLCPACKAGES) \
	  -I +camlp4 -pp camlp4of -c $<

pa_bitstring.cmo: pa_bitstring.ml bitstring.cma bitstring_persistent.cma
	$(OCAMLFIND) ocamlc bitstring.cma -I +camlp4 camlp4lib.cma \
	  -pp camlp4of -c $< -o $@

bitstring-objinfo: bitstring_objinfo.cmo bitstring.cma bitstring_persistent.cma
	$(OCAMLFIND) ocamlc -I +camlp4 unix.cma camlp4lib.cma \
	  $(OCAMLCLIBS) \
	  bitstring.cma bitstring_persistent.cma \
	  $< -o $@

# Create byteswap.h if the system doesn't have this file.
# From gnulib, license is LGPLv2+.

byteswap.h: byteswap.in.h
	{ echo '/* DO NOT EDIT! GENERATED AUTOMATICALLY! */'; \
	  cat $(srcdir)/byteswap.in.h; \
	} > $@-t
	mv -f $@-t $@

# Clean.

clean:
	rm -f core *~ *.cmi *.cmo *.cmx *.cma *.cmxa *.cmp *.a *.o *.so
	rm -f tests/*~ tests/*.cmi tests/*.cmo tests/*.cmx tests/*.opt
	rm -f tests/*.o
	rm -f examples/*~ examples/*.cmi examples/*.cmo examples/*.cmx
	rm -f examples/*.o $(EXAMPLES)
	rm -f benchmarks/*~ benchmarks/*.cmi benchmarks/*.cmo
	rm -f benchmarks/*.cmx benchmarks/*.o
	rm -f benchmarks/*.opt benchmarks/*.opt.p benchmarks/*.profile
	rm -f create_test_pattern bitstring-objinfo
	@for d in $(SUBDIRS); do $(MAKE) -C $$d $@; done

# Install.

install:
	ocamlfind install bitstring META *.mli *.cmx *.cma *.cmxa *.a *.so \
		bitstring.cmi \
		bitstring_persistent.cmi \
		pa_bitstring.cmo
	@for d in $(SUBDIRS); do $(MAKE) -C $$d $@; done

# Standard rules.

.mli.cmi:
	$(OCAMLFIND) ocamlc $(OCAMLCFLAGS) $(OCAMLCPACKAGES) -c $<
.ml.cmo:
	$(OCAMLFIND) ocamlc $(OCAMLCFLAGS) $(OCAMLCPACKAGES) -c $<
.ml.cmx:
	$(OCAMLFIND) ocamlopt $(OCAMLOPTFLAGS) $(OCAMLOPTPACKAGES) -c $<

depend: .depend
	@for d in $(SUBDIRS); do $(MAKE) -C $$d $@; done

.depend: bitstring.ml bitstring.mli \
	  bitstring_persistent.ml bitstring_persistent.mli \
	  bitstring_objinfo.ml \
	  create_test_pattern.ml
	rm -f .depend
	$(OCAMLFIND) ocamldep $(OCAMLCPACKAGES) \
	  bitstring.ml bitstring.mli \
	  bitstring_objinfo.ml >> $@
	$(OCAMLFIND) ocamldep $(OCAMLCPACKAGES) -pp camlp4of \
	  bitstring_persistent.ml bitstring_persistent.mli \
	  create_test_pattern.ml >> $@

ifeq ($(wildcard .depend),.depend)
include .depend
endif

.PHONY: depend dist check-manifest dpkg doc \
	print-examples print-tests examples test benchmarks

.SUFFIXES:      .cmo .cmi .cmx .ml .mli .mll
