.PHONY: doc test all install uninstall clean distclean

prefix = $(shell opam config var prefix)

all:
	ocaml pkg/pkg.ml build

clean:
	ocaml pkg/pkg.ml clean

doc:
	ocamlbuild \
	    doc/panograph.docdir/index.html \
	    doc/panograph-server.docdir/index.html \
	    doc/panograph-client.docdir/index.html

test:
	ocaml pkg/pkg.ml build
	ocaml pkg/pkg.ml test

install:
	opam-installer --prefix $(prefix) panograph.install

uninstall:
	opam-installer --prefix $(prefix) -u panograph.install

distclean: clean
	rm -f panograph.install
