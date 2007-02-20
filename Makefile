
# OCaml programs for compiling
OCAMLDEP = ocamldep
OCAMLC = ocamlc
OCAMLOPT = ocamlopt
OCAMLFIND = ocamlfind

# Libs to link with
REQUIRES = unix str

# Include dirs
INCLUDES = -I lib

# Install prefix for client
PREFIX = /usr/local

# Sources
LIBSRC = lib/fcall.ml lib/ixpc.ml
CLSRC = src/client.ml

# The output to create
CLIENT = oixpc
LIB = ixp.cma
LIBX = ixp.cmxa

NAME=ixp

# Automagic stuff below

LIBOBJ = $(patsubst %.ml,%.cmo,$(LIBSRC))
LIBXOBJ = $(patsubst %.ml,%.cmx,$(LIBSRC))
CLOBJ = $(patsubst %.ml,%.cmx,$(CLSRC))
LIBIF = $(patsubst %.ml,%.cmi,$(LIBSRC))

all: $(LIB) $(LIBX) $(CLIENT)

.PHONY: install
install: all
	$(OCAMLFIND) install $(NAME) $(LIB) $(LIBIF) $(LIBX) META
	install $(CLIENT) -d $(DESTDIR)$(PREFIX)/bin/

.PHONY: uninstall
uninstall:
	$(OCAMLFIND) remove $(NAME)
	rm -f $(DESTDIR)$(PREFIX)/bin/$(CLIENT)

$(LIB): $(LIBOBJ)
	$(OCAMLFIND) $(OCAMLC) -a -o $@ -package "$(REQUIRES)" -linkpkg \
	  $^

$(LIBX): $(LIBXOBJ)
	$(OCAMLFIND) $(OCAMLOPT) -a -o $@ -package "$(REQUIRES)" \
	   $^

$(CLIENT): $(CLOBJ)
	$(OCAMLFIND) $(OCAMLOPT) -package "$(REQUIRES)" -linkpkg -o $@ $(LIBX) $^

%.cmo: %.ml
	$(OCAMLFIND) $(OCAMLC) -c $(INCLUDES) -package "$(REQUIRES)" $<

%.cmx: %.ml
	$(OCAMLFIND) $(OCAMLOPT) -c $(INCLUDES) -package "$(REQUIRES)" $<

.PHONY: clean
clean:
	rm -f lib/*.cmo lib/*.cmx lib/*.cmi lib/*.o src/*.cmx src/*.cmi src/*.o \
	  $(LIB) $(LIBX) $(patsubst %.cmxa,%.a,$(LIBX)) $(CLIENT)
