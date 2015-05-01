
SOURCES= hpp.ml ptbparser.mly lexer.mll ptbtree.ml tree.ml \
rule.ml rule.mli cky.ml treebank.ml treebank.mli \
ckygram.ml ckygram.mli

all: hpp.native hpp.byte

hpp.native: $(SOURCES)
	corebuild -use-menhir -package re2 hpp.native


hpp.byte: $(SOURCES)
	corebuild -use-menhir -package re2 hpp.byte

clean:
	-rm -rf _build
	-rm hpp.native
	-rm hpp.byte


utop: hpp.byte
	utop -init ocamlinit -I . -I _build
