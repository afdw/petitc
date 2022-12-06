all:
	dune build bin

test:
	dune runtest

clean:
	dune clean

.PHONY: all test clean
