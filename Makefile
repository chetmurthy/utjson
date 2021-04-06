
OCAMLFIND=ocamlfind
NOT_OCAMLFIND=not-ocamlfind
BASEPACKAGES=bos,uutf,fmt,camlp5.extprint,pcre,yaml
PACKAGES=$(BASEPACKAGES),camlp5.extprint,camlp5.extend,camlp5.pprintf,pa_ppx.deriving_plugins.std,pa_ppx.base.link,pa_ppx.runtime,pa_ppx.testutils,sedlex

OBJ=utypes.cmo utparse0.cmo
OML=utypes.m
LEXML=
RML=utparse0.ml

all: $(OBJ) uttest

test:: all
	mkdir -p _build
	./uttest

uttest: $(OBJ) uttest.cmo
	$(OCAMLFIND) ocamlc $(DEBUG) -package $(PACKAGES),oUnit -linkpkg -linkall -syntax camlp5r $^ -o $@

utparse0.cmo: utparse0.ml
	$(OCAMLFIND) ocamlc $(DEBUG) -package $(PACKAGES) -syntax camlp5r -c $<

.SUFFIXES: .mll .ml .cmo .cmx

.ml.cmo:
	$(OCAMLFIND) ocamlc $(DEBUG) -package $(PACKAGES) -syntax camlp5o -c $<

clean:
	rm -rf *test *.cm* *.o _build *.log *.cache


depend::
	$(OCAMLFIND) ocamldep $(DEBUG) -package $(PACKAGES) -syntax camlp5o \
		$(OML) \
		 > .depend.NEW
	$(OCAMLFIND) ocamldep $(DEBUG) -package sedlex.ppx \
		$(LEXML) \
		 >> .depend.NEW
	$(OCAMLFIND) ocamldep $(DEBUG) -package $(PACKAGES) -syntax camlp5r \
		$(RML) \
		 >> .depend.NEW
	mv .depend.NEW .depend

-include .depend
