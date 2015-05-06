SOURCES= parser/hpp.ml utils/ptbparser.mly utils/lexer.mll parser/ptbtree.ml parser/tree.ml \
parser/rule.ml parser/rule.mli parser/cky.ml parser/cky.mli parser/treebank.ml parser/treebank.mli \
parser/ckygram.ml parser/ckygram.mli \
parser/backpointer.ml parser/backpointer.mli

all: hpp.native hpp.byte

hpp.native: $(SOURCES)
	corebuild -use-menhir -package re2 parser/hpp.native


hpp.byte: $(SOURCES)
	corebuild -use-menhir -package re2 parser/hpp.byte

clean:
	-rm -rf _build
	-rm hpp.native
	-rm hpp.byte


utop: hpp.byte
	utop -init ocamlinit -I . -I _build
