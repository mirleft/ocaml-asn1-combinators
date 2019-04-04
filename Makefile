.PHONY: all test doc clean

all:
	dune build

test:
	dune runtest

doc:
	dune build @doc

clean:
	dune clean
