all: tests
.PHONY: tests
tests:
	echo 'CM.make "sources.cm"' | sml
