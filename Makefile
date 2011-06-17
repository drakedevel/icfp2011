all: ltg
.PHONY: all tests ltg

tests:
	echo 'CM.make "sources.cm"' | sml

bin/ltg: FORCE
	echo 'use "bin/compile-ltg.sml";' | sml
ltg: bin/ltg


sources.mlb: sources.cm mlton-parse
	${RM} sources.mlb
	cp sources.cm sources-mlton.cm
	echo "go.sml" >> sources-mlton.cm
	cm2mlb sources-mlton.cm > sources.mlb

FORCE: 
