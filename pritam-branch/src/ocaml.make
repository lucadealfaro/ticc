OCAMLC=ocamlc
OCAMLOPT=ocamlopt
OCAMLMKTOP=ocamlmktop
# OCAMLMKTOP=ocamlc -make-runtime -linkall
OCAMLLEX=ocamllex
OCAMLYACC=ocamlyacc
OCAMLDEP=ocamldep
OCAMLMKLIB=ocamlmklib
OCAMLDOC=ocamldoc


SUFFIXES= .ml .mll .mly .cmo .cmx .cmi


%.o: %.c
	@echo '$(OCAMLC) -c $(OCAML_CCFLAGS) $<'; \
	$(OCAMLC) -c $(OCAML_CCFLAGS) $<

%.cmo: %.ml
	@echo '$(OCAMLC) $(OCAML_CFLAGS) -c $<'; \
	$(OCAMLC) $(OCAML_CFLAGS) -c $<

%.cmi: %.mli
	@echo '$(OCAMLC) $(OCAML_CFLAGS) -c $<'; \
	$(OCAMLC) $(OCAML_CFLAGS) -c $<

%.ml: %.mll
	@echo '$(OCAMLLEX) $<'; \
	$(OCAMLLEX) $<

%.ml: %.mly
	@echo '$(OCAMLYACC) $<'; \
	$(OCAMLYACC) $<

%.mli: %.mly
	@echo '$(OCAMLYACC) $<'; \
	$(OCAMLYACC) $<

