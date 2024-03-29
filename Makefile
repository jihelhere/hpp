PSOURCES= parser/hpp.ml utils/ptbparser.mly utils/lexer.mll parser/ptbtree.ml parser/tree.ml \
parser/rule.ml parser/rule.mli parser/cky.ml parser/cky.mli parser/treebank.ml parser/treebank.mli \
parser/ckygram.ml parser/ckygram.mli \
parser/backpointer.ml parser/backpointer.mli

TSOURCES= tagger/hpt.ml tagger/conlltag.ml tagger/conlltag.mli \
	utils/conll.mli utils/decoder.mli utils/eval.mli utils/feature.mli \
	utils/int2stringmap.ml utils/int2stringmap.mli utils/perceptrontrainer.ml \
	utils/perceptrontrainer.mli utils/template.mli \
	tagger/templatetag.ml tagger/featuretag.ml \
	tagger/modeltag.ml tagger/sequencedecoder.ml tagger/evaltag.ml

all: hpp.native hpt.native

hpp.native: $(PSOURCES)
	corebuild -cflags -ccopt,-O9 -use-menhir -package re2 parser/hpp.native


hpt.native: $(TSOURCES)
	corebuild -cflags -unsafe,-ccopt,-O9 -package re2 tagger/hpt.native


hpp.byte: $(PSOURCES)
	corebuild -use-menhir -package re2 parser/hpp.byte

clean:
	-rm -rf _build
	-rm *.native
	-rm *.byte


utop: hpp.byte
	utop -init ocamlinit -I . -I _build
