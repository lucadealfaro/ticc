ifndef TICCSRC
TICCSRC=$(shell pwd)
export TICCSRC
$(warning Warning: TICCSRC environment variable is not set!)
$(warning Warning: *guessing* it should be $(TICCSRC))
endif

include ../ocaml.make

GLUDIR = ../glu-2.0
GLUINCDIR = $(GLUDIR)/include


# Vars we use in our rules to build ocaml programs
OCAMLDEBUG = -g
INCLUDES=-I ../mlglu
OCAML_CFLAGS=$(INCLUDES) $(OCAMLDEBUG) 
OCAMLOPTFLAGS=$(INCLUDES)
OCAMLMKTOPFLAGS=$(INCLUDES) $(OCAMLDEBUG) 
OCAML_CCFLAGS=-custom $(OCAMLOPTFLAGS) str.cma -cclib -lstr
OCAML_CFLAGS=-custom $(OCAMLOPTFLAGS) str.cma

.PHONY: all clean
all: ticc

#
# Config for the 'ticc' program.
#

ticc_OBJS = hsetmap.cmo biject.cmo var.cmo ast.cmo rule.cmo \
	mod.cmo prog.cmo typecheck.cmo ticlex.cmo ticparse.cmo \
	symtemp.cmo symmod.cmo \
	symprog.cmo symutil.cmo symbuild.cmo symops.cmo ticc.cmo
ticc_LD_ADD = ../mlglu/mlglu.cma str.cma -cclib -lstr



ticctop: $(ticc_OBJS)
	@rm -f ticc
	$(OCAMLMKTOP) -custom -o ticctop $(OCAMLMKTOPFLAGS) $(ticc_LD_ADD) $(ticc_OBJS)


ticc: 	ticctop
	sed -e 's;@TICCSRC@;$(TICCSRC);g' ticc.in > ticc
	chmod a+rx ticc

#
# Boilerplate code for building ocaml dependencies, and
# "make clean".
#

clean:
	rm -f ticc ticctop .depends *.cmo *.cmx *.cmi ticparse.mli ticparse.ml ticlex.ml
	rm -rf $(TICCSRC)/../../doc/api

.depends: *.ml *.mli ticparse.ml ticlex.ml
	$(OCAMLDEP) $^ > $@

-include .depends

