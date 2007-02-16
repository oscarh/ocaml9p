OCAMLMAKEFILE = ./OCamlMakefile

LIBS = unix str

RESULT = ixp
SOURCES = src/fcall.ml src/ixp.ml
all: native-code

include $(OCAMLMAKEFILE)
