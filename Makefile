all:
	dune build lib/ztl.cmxa
	
clean:
	dune clean

.PHONY: all clean
