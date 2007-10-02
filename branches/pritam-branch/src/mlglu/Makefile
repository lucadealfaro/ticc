include ../ocaml.make

GLUDIR = ../glu-2.0
GLUINCDIR = $(GLUDIR)/include

# build variable
OCAML_CCFLAGS=-I $(GLUINCDIR)

libmlglu_a_OBJS = vset.cmo mlglu.cmo util.o mdd_minmax.o mlglu.o

.PHONY: all clean test
all: libmlglu.a

clean:
	rm -f *.a *.so .depends *.cma *.cmo *.cmx *.cmi *.o

test:
	ocamlmktop -o mlglu $(libmlglu_a_OBJS) -custom -cclib -L../glu-2.0 -cclib -lcu -cclib -lglu                                          

libmlglu.a: $(libmlglu_a_OBJS)
	-rm -f libmlglu.a
	-rm -f dllmlglu.so
	-rm -f mlglu.cma
	$(OCAMLMKLIB) -o mlglu $(libmlglu_a_OBJS) -cclib -L$(GLUDIR) -cclib -lcu -cclib -lglu


.depends: *.ml *.mli
	$(OCAMLDEP) $^ > $@

-include .depends

