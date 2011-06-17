all: ltg
.PHONY: all tests ltg mlton-ltg tar

MLTON = mlton -const "Exn.keepHistory true" -default-ann "redundantMatch warn" -default-ann "sequenceNonUnit warn" -output

tests:
	echo 'CM.make "sources.cm"' | sml

bin/ltg: FORCE
	echo 'use "bin/compile-ltg.sml";' | sml
ltg: bin/ltg

bin/mlton-ltg: sources.mlb FORCE
	$(MLTON) bin/mlton-ltg sources.mlb
mlton-ltg: bin/mlton-ltg

sources.mlb: sources.cm cm2mlb/cm2mlb.x86-linux
	${RM} sources.mlb
	cp sources.cm sources-mlton.cm
	echo "go.sml" >> sources-mlton.cm
	cm2mlb/cm2mlb sources-mlton.cm > sources.mlb

cm2mlb/cm2mlb.x86-linux:
	make -C cm2mlb

mlton-l4c: sources.mlb mlton-parse FORCE
	$(MLTON) $(MLTON_TARG) sources.mlb

tar: ltg.tar.gz

ltg.tar.gz: bin/mlton-ltg
	./build.sh $@

FORCE: 
