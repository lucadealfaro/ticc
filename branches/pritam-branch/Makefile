
VERSION=`cat VERSION`
SRCDIR = src
PARSEDIR = $(SRCDIR)/ticc
GLUDIR = $(SRCDIR)/mlglu
DOCDIR = doc/api

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
	rm -rf doc

doc:
	rm -rf $(DOCDIR)
	mkdir -p doc/api
	$(OCAMLDOC) -html -d $(DOCDIR) -I $(GLUDIR) -I $(PARSEDIR) $(PARSEDIR)/*.mli $(PARSEDIR)/*.ml

dist: apidoc
	rm -rf ticc-$(VERSION)
	mkdir -p ticc-$(VERSION)
	@echo Creating temporary build replica
	tar -cf - --exclude .svn --exclude .cvsignore \
		--exclude "*.cmi" --exclude "*.cmo" \
		--exclude "*.out" --exclude "*.diff" \
		--exclude "doc/"  --exclude "*~" \
		--exclude "*.o" --exclude "*.a" --exclude "examples/etc" \
		--exclude "*.tgz" \
		--exclude "ticc-$(VERSION)" \
		. | tar -C ticc-$(VERSION) -xf -
	@echo Building tar distribution: ticc-$(VERSION).tar.gz
	rm -f ticc-$(VERSION).tar ticc-$(VERSION).tar.gz
	tar cvfz ticc-$(VERSION).tgz ticc-$(VERSION)
	rm -rf ticc-$(VERSION)




