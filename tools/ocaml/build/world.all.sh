#!/bin/sh
# $Id: world.all.sh,v 1.2.4.1 2007/03/12 11:58:48 pouillar Exp $
set -e
cd `dirname $0`/..
. build/targets.sh
set -x
$OCAMLBUILD $@ \
  $STDLIB_BYTE $OCAMLC_BYTE $OCAMLLEX_BYTE $OCAMLOPT_BYTE $TOPLEVEL \
  $TOOLS_BYTE $OTHERLIBS_BYTE $OCAMLBUILD_BYTE $DEBUGGER $OCAMLDOC_BYTE \
  $CAMLP4_BYTE $STDLIB_NATIVE $OCAMLC_NATIVE $OCAMLOPT_NATIVE \
  $OCAMLLEX_NATIVE $TOOLS_NATIVE $OTHERLIBS_NATIVE \
  $OCAMLBUILD_NATIVE $OCAMLDOC_NATIVE $CAMLP4_NATIVE
