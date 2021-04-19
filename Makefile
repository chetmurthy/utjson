DEBUG=-g
OCAMLFIND=ocamlfind
NOT_OCAMLFIND=not-ocamlfind
BASEPACKAGES=bos,uutf,fmt,camlp5.extprint,pcre,yaml
PACKAGES=$(BASEPACKAGES),camlp5.extprint,camlp5.extend,camlp5.pprintf,pa_ppx.utils,pa_ppx.deriving_plugins.std,pa_ppx.base.link,pa_ppx.runtime,pa_ppx.testutils,sedlex

IMPORT_PACKAGES = pa_ppx_migrate,pa_ppx.import
IMPORT_OCAMLCFLAGS = -ppopt -pa_import-I -ppopt . -ppopt -pa_passthru-debug


OBJ=ututil.cmo utypes.cmo utlexing.cmo utmigrate.cmo utparse0.cmo utprint.cmo \
    utio.cmo utconv.cmo uttypecheck.cmo uteval.cmo
OML=ututil.ml uttestutil.ml utio.ml utconv.ml uttypecheck.ml uteval.ml \
    syntax_test.ml schemastore_test.ml typing_test.ml eval_test.ml
IMPORT_OML=utypes.ml utmigrate.ml
LEXML=utlexing.ml
RML=utparse0.ml utprint.ml

all: $(OBJ) syntax_test typing_test schemastore_test eval_test

doc: README.html

README.html: README.rst
	pandoc -t html $< > README.html

test:: all
	rm -rf _build && mkdir -p _build
	./syntax_test
	./typing_test
	./schemastore_test || true
	./eval_test

utjtool.TEST: utjtool
	./utjtool --help
	./utjtool convert --utj-path _build:.git /dev/zero
	./utjtool convert --utj-path _build:.git --utj-path _build:.git /dev/zero
	UTJPATH=_build:.git ./utjtool convert /dev/zero
	./utjtool convert --utj-path _build:.git --utj-path _build:.git -o /tmp /dev/zero /dev/null

utjtool:: utjtool.ml
	$(OCAMLFIND) ocamlc $(DEBUG) $(OCAMLFLAGS) -package yojson,str,sedlex,pa_ppx.runtime,pa_ppx.base.link,fmt,bos,ppx_deriving.show,ppx_deriving_cmdliner,cmdliner -linkpkg -linkall $(OBJ) utjtool.ml -o utjtool

syntax_test: $(OBJ) uttestutil.cmo syntax_test.cmo
	$(OCAMLFIND) ocamlc $(DEBUG) $(OCAMLCFLAGS) -package $(PACKAGES),oUnit -linkpkg -linkall -syntax camlp5r $^ -o $@

typing_test: $(OBJ) uttestutil.cmo typing_test.cmo
	$(OCAMLFIND) ocamlc $(DEBUG) $(OCAMLCFLAGS) -package $(PACKAGES),oUnit -linkpkg -linkall -syntax camlp5r $^ -o $@

schemastore_test: $(OBJ) uttestutil.cmo schemastore_test.cmo
	$(OCAMLFIND) ocamlc $(DEBUG) $(OCAMLCFLAGS) -package $(PACKAGES),oUnit -linkpkg -linkall -syntax camlp5r $^ -o $@

eval_test: $(OBJ) uttestutil.cmo eval_test.cmo
	$(OCAMLFIND) ocamlc $(DEBUG) $(OCAMLCFLAGS) -package $(PACKAGES),oUnit -linkpkg -linkall -syntax camlp5r $^ -o $@

utmigrate.cmo: utmigrate.ml
	$(OCAMLFIND) ocamlc $(DEBUG) $(OCAMLCFLAGS) $(IMPORT_OCAMLCFLAGS) -package $(PACKAGES),$(IMPORT_PACKAGES) -syntax camlp5o -c $<

utypes.cmo: utypes.ml
	$(OCAMLFIND) ocamlc $(DEBUG) $(OCAMLCFLAGS) $(IMPORT_OCAMLCFLAGS) -package $(PACKAGES),$(IMPORT_PACKAGES) -syntax camlp5o -c $<

utypes.cmi: utypes.mli
	$(OCAMLFIND) ocamlc $(DEBUG) $(OCAMLCFLAGS) $(IMPORT_OCAMLCFLAGS) -package $(PACKAGES),$(IMPORT_PACKAGES) -syntax camlp5o -c $<

utparse0.cmo: utparse0.ml
	$(OCAMLFIND) ocamlc $(DEBUG) $(OCAMLCFLAGS) -package $(PACKAGES) -syntax camlp5r -c $<

utprint.cmo: utprint.ml
	$(OCAMLFIND) ocamlc $(DEBUG) $(OCAMLCFLAGS) -package $(PACKAGES) -syntax camlp5r -c $<

utlexing.cmo: utlexing.ml
	$(OCAMLFIND) ocamlc $(DEBUG) $(OCAMLCFLAGS) -package $(BASEPACKAGES),sedlex.ppx -c $<

.SUFFIXES: .mll .ml .cmo .cmx

.ml.cmo:
	$(OCAMLFIND) ocamlc $(DEBUG) $(OCAMLCFLAGS) -package $(PACKAGES) -syntax camlp5o -c $<

clean:
	rm -rf *test *.cm* *.o _build *.log *.cache


depend::
	$(OCAMLFIND) ocamldep $(OCAMLCFLAGS) -package $(PACKAGES) -syntax camlp5o \
		$(OML) \
		 > .depend.NEW
	$(OCAMLFIND) ocamldep $(OCAMLCFLAGS) $(IMPORT_OCAMLCFLAGS) -package $(PACKAGES),$(IMPORT_PACKAGES) -syntax camlp5o \
		$(IMPORT_OML) \
		 >> .depend.NEW
	$(OCAMLFIND) ocamldep $(OCAMLCFLAGS) -package sedlex.ppx \
		$(LEXML) \
		 >> .depend.NEW
	$(OCAMLFIND) ocamldep $(OCAMLCFLAGS) -package $(PACKAGES) -syntax camlp5r \
		$(RML) \
		 >> .depend.NEW
	mv .depend.NEW .depend

-include .depend
