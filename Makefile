UNAME := $(shell uname)
ifeq ($(UNAME), Linux)
  FORMAT=aout
else
ifeq ($(UNAME), Darwin)
  FORMAT=macho
endif
endif

BUILD=ocamlbuild -use-ocamlfind
SRC=src
TEST=test

main: $(SRC)/lexer.mll $(SRC)/parser.dyp $(SRC)/main.ml $(SRC)/ast.ml $(SRC)/astAnf.ml $(SRC)/pPrint.ml
	$(BUILD) src/main.native
	mv main.native main

.PHONY: deps
deps:
	opam install oUnit dypgen extlib ANSITerminal batteries sexplib

lex: $(SRC)/lexer.mll $(SRC)/parser.dyp $(SRC)/lex.ml
	$(BUILD) src/lex.native
	mv lex.native lex

run-tests: $(TEST)/run_tests.ml $(TEST)/pPrintTest.ml $(TEST)/testUtils.ml $(TEST)/lexerTest.ml $(TEST)/parserTest.ml $(TEST)/desugarTest.ml
	$(BUILD) test/run_tests.native
	mv run_tests.native run-tests

show-parse: $(SRC)/showParse.ml
	$(BUILD) src/showParse.native
	mv showParse.native show-parse

clean:
	$(BUILD) -clean
