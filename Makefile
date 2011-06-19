all: ltg
.PHONY: all tests ltg mlton-ltg tar

#MLTON = mlton -const "Exn.keepHistory true" -default-ann "redundantMatch warn" -default-ann "sequenceNonUnit warn" -output
MLTON = mlton -default-ann "nonexhaustiveMatch ignore" -default-ann "sequenceNonUnit ignore" -output

tests:
	echo 'CM.make "sources.cm"' | sml

bin/ltg.x86-linux:  *.sml
	echo 'use "bin/compile-ltg.sml";' | sml
ltg: bin/ltg.x86-linux

bin/mlton-ltg-bin: sources.mlb *.sml
	$(MLTON) bin/mlton-ltg-bin sources.mlb
mlton-ltg: bin/mlton-ltg-bin

sources.mlb: sources.cm cm2mlb/cm2mlb.x86-linux *.sml
	${RM} sources.mlb
	cp sources.cm sources-mlton.cm
	echo "go.sml" >> sources-mlton.cm
	cm2mlb/cm2mlb sources-mlton.cm > sources.mlb

cm2mlb/cm2mlb.x86-linux:
	make -C cm2mlb

tar: ltg.tar.gz

ltg.tar.gz: bin/mlton-ltg-bin bin/mlton-ltg
	./build.sh $@

FORCE: 
