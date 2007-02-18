OCAMLMAKEFILE = ./OCamlMakefile

LIBS = unix str

RESULT = ixp
SOURCES = lib/fcall.ml lib/ixpc.ml src/client.ml
all: native-code

include $(OCAMLMAKEFILE)
