export OCAMLMAKEFILE = ./OCamlMakefile

all: ixp.cmxa oixpc 

ixp.cmxa:
	@make -f Makefile_lib

oixpc:
	@make -f Makefile_client

.PHONY: clean
clean:
	@make -f Makefile_lib clean
	@make -f Makefile_client clean
