
# OCaml programs for compiling
OCAMLDEP = ocamldep
OCAMLC = ocamlc
OCAMLOPT = ocamlopt
OCAMLFIND = ocamlfind

# Libs to link with
LIBS = unix.cma str.cma
LIBSX = unix.cmxa str.cmxa

# Include dirs
INCLUDES = -I lib

# Install prefix for client
PREFIX = /usr/local/

# Sources
LIBSRC = lib/fcall.ml lib/ixpc.ml
CLSRC = src/client.ml

# The output to create
CLIENT = oixpc
LIB = ixp.cma
LIBX = ixp.cmxa

# Automagic stuff below

LIBOBJ = $(patsubst %.ml,%.cmo,$(LIBSRC))
LIBXOBJ = $(patsubst %.ml,%.cmx,$(LIBSRC))
CLOBJ = $(patsubst %.ml,%.cmx,$(CLSRC))

all: $(LIB) $(LIBX) $(CLIENT)

$(LIB): $(LIBOBJ)
	$(OCAMLFIND) $(OCAMLC) -a -o $@ $^

$(LIBX): $(LIBXOBJ)
	$(OCAMLFIND) $(OCAMLOPT) -a -o $@ $^

$(CLIENT): $(CLOBJ)
	$(OCAMLFIND) $(OCAMLOPT) -o $@ $(LIBSX) $(LIBX) $^

%.cmo: %.ml
	$(OCAMLFIND) $(OCAMLC) -c $(INCLUDES) $<

%.cmx: %.ml
	$(OCAMLFIND) $(OCAMLOPT) -c $(INCLUDES) $<

.PHONY: clean
clean:
	rm -f lib/*.cmo lib/*.cmx lib/*.cmi lib/*.o src/*.cmx src/*.cmi \
	  src/*.o $(LIB) $(LIBX) $(patsubst %.cmxa,%.a,$(LIBX)) $(CLIENT)
