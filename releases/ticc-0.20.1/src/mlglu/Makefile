include ../ocaml.make

GLUDIR = ../glu-2.0
GLUINCDIR = $(GLUDIR)/include

# build variable
OCAML_CCFLAGS=-I $(GLUINCDIR)

.PHONY: all clean test
all: libmlglu.a

clean:
	rm -f *.a *.so .depends *.cma *.cmo *.cmx *.cmi *.o

test:
	ocamlmktop -o mlglu vset.cmo mlglu.cmo util.o mlglu.o -custom -cclib -L../glu-2.0 -cclib -lcu -cclib -lglu                                          

libmlglu_a_OBJS = vset.cmo mlglu.cmo util.o mlglu.o
libmlglu.a: $(libmlglu_a_OBJS)
	-rm -f libmlglu.a
	-rm -f dllmlglu.so
	-rm -f mlglu.cma
	$(OCAMLMKLIB) -o mlglu $(libmlglu_a_OBJS) -cclib -L$(GLUDIR) -cclib -lcu -cclib -lglu


.depends: *.ml *.mli
	$(OCAMLDEP) $^ > $@

-include .depends

