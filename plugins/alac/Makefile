
all: alac32.native alt.native

alt.native: arrayTypes.cmi bigutils.cmxa matrix.cmx dynamicPredictor.cmx bitBuffer.cmx adaptiveGolomb.cmx mp4_alt.cmx alac_alt.cmx
	../../tools/custom/bin/ocamlopt.opt -I +bitstring unix.cmxa bitstring.cmxa bigarray.cmxa bigutils.cmxa $(filter-out %.cmi, $+) -I . -o $@

alac32.native: arrayTypes.cmi bigutils.cmxa matrix.cmx dynamicPredictor.cmx bitBuffer.cmx adaptiveGolomb.cmx mp4.cmx alac.cmx
	../../tools/custom/bin/ocamlopt.opt -I +bitstring unix.cmxa bitstring.cmxa bigarray.cmxa $(filter-out %.cmi, $+) -I . -o $@

bigutils.cmxa: arrayTypes.cmi bigarray_extra_stubs.o bigarrayUtils.cmx
	../../tools/custom/bin/ocamlmklib -failsafe $(filter-out %.cmi, $+) -o $(basename $@)

clean:
	rm -f *.cm[aoix] *.cmxa *.[ao] *.so alac32.native

%.cmx: %.ml
	../../tools/custom/bin/ocamlopt.opt -inline 100 -noassert  -I +bitstring -c $< -o $@

%.ml: %.mlp
	camlp4o -I `ocamlc -where`/bitstring bitstring.cma bitstring_persistent.cma pa_bitstring.cmo -printer Camlp4OCamlPrinter -impl $< > $@

%.cmi: %.mli
	../../tools/custom/bin/ocamlopt.opt -inline 100 -noassert  -c $< -o $@

%.o: %.c
	../../tools/custom/bin/ocamlopt.opt -inline 100 -noassert  -c $< -o $@
