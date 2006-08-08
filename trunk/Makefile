
VERSION=`cat VERSION`
SRCDIR = src
PARSEDIR = $(SRCDIR)/ticc
GLUDIR = $(SRCDIR)/mlglu

include $(SRCDIR)/ocaml.make

.PHONY: all ticc clean apidoc dist

all: ticc

ticc:   
	cd $(GLUDIR); make
	cd $(PARSEDIR); make
	ln -fs $(SRCDIR)/ticc/ticc ticc

clean:
	cd $(GLUDIR); make clean
	cd $(PARSEDIR); make clean

doc:
	rm -rf doc/api
	mkdir -p doc/api
	$(OCAMLDOC) -html -d doc/api $(PARSEDIR)/ticc.mli

dist: apidoc
	rm -rf ticc-$(VERSION)
	@echo Building tar distribution: ticc-$(VERSION).tgz
	tar cfvz ticc-$(VERSION).tgz \
		--exclude .svn    --exclude .cvsignore \
		--exclude "*.cmi" --exclude "*.cmo" \
		--exclude "*.out" --exclude "*.diff" \
		--exclude "doc/"  --exclude "*~" \
		--exclude "*.o" --exclude "*.a" --exclude "examples/etc" \
		--exclude "*.tgz" \
		--exclude "ticc-$(VERSION)" \
		.



