
OCAMLFIND=ocamlfind
NOT_OCAMLFIND=not-ocamlfind
BASEPACKAGES=bos,uutf,fmt,camlp5.extprint,pcre,yaml
PACKAGES=$(BASEPACKAGES),camlp5.extprint,camlp5.extend,camlp5.pprintf,pa_ppx.deriving_plugins.std,pa_ppx.base.link,pa_ppx.runtime,pa_ppx.testutils,sedlex

OBJ=ututil.cmo utypes.cmo utlexing.cmo utparse0.cmo utprint.cmo utconv.cmo uttypecheck.cmo utvalid0.cmo
OML=ututil.ml utypes.ml syntax_test.ml utconv.ml uttypecheck.ml utvalid0.ml
LEXML=utlexing.ml
RML=utparse0.ml utprint.ml

all: $(OBJ) syntax_test

test:: all
	rm -rf _build && mkdir -p _build
	./syntax_test

syntax_test: $(OBJ) syntax_test.cmo
	$(OCAMLFIND) ocamlc $(DEBUG) -package $(PACKAGES),oUnit -linkpkg -linkall -syntax camlp5r $^ -o $@

utparse0.cmo: utparse0.ml
	$(OCAMLFIND) ocamlc $(DEBUG) -package $(PACKAGES) -syntax camlp5r -c $<

utprint.cmo: utprint.ml
	$(OCAMLFIND) ocamlc $(DEBUG) -package $(PACKAGES) -syntax camlp5r -c $<

utlexing.cmo: utlexing.ml
	$(OCAMLFIND) ocamlc $(DEBUG) -package $(BASEPACKAGES),sedlex.ppx -c $<

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
