
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
	mkdir -p ticc-$(VERSION)
	@echo Creating temporary build replica
	tar -cf - --exclude .svn \
		--exclude "*.cmi" --exclude "*.cmo" \
		--exclude "*.o" --exclude "*.a" --exclude "examples/etc" \
		--exclude "*.tar" --exclude "*.tar.gz" \
		--exclude "ticc-$(VERSION)" \
		. | tar -C ticc-$(VERSION) -xf -
	@echo Building tar distribution: ticc-$(VERSION).tar.gz
	rm -f ticc-$(VERSION).tar ticc-$(VERSION).tar.gz
	tar -cvf ticc-$(VERSION).tar ticc-$(VERSION)
	gzip --best ticc-$(VERSION).tar
	rm -rf ticc-$(VERSION)

