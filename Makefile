all:
	dune build lib/ztl.cmxa
	
clean:
	dune clean

test:
	dune runtest

metadata:
	dune build @install

.PHONY: all clean test metadata
